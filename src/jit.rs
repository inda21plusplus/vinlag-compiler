use crate::parser::{get_string_id, Expr, ParserFunction};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use std::collections::HashMap;
use std::slice;
/// The basic JIT class.
pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data context, which is to data objects what `ctx` is to functions.
    data_ctx: DataContext,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,
}

impl Default for JIT {
    fn default() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }
}

impl JIT {
    /// Compile a string in the language into machine code.
    pub fn compile(&mut self, input: Vec<(bool, ParserFunction)>) -> Result<*const u8, String> {
        let mut ptr: Option<*const u8> = None;
        for (is_main, fun) in input {
            // Then, translate the AST nodes into Cranelift IR.Eq
            self.translate(fun.arguments, fun.stmts)?;

            // Next, declare the function to jit. Functions must be declared
            // before they can be called, or defined.
            let id = self
                .module
                .declare_function(&fun.name, Linkage::Export, &self.ctx.func.signature)
                .map_err(|e| e.to_string())?;

            // Define the function to jit. This finishes compilation, although
            // there may be outstanding relocations to perform. Currently, jit
            // cannot finish relocations until all functions to be called are
            // defined.
            self.module
                .define_function(
                    id,
                    &mut self.ctx,
                    &mut codegen::binemit::NullTrapSink {},
                    &mut codegen::binemit::NullStackMapSink {},
                )
                .map_err(|e| e.to_string())?;

            // Now that compilation is finished, we can clear out the context state.
            self.module.clear_context(&mut self.ctx);

            // Finalize the functions which we just defined, which resolves any
            // outstanding relocations (patching in addresses, now that they're
            // available).
            self.module.finalize_definitions();

            if is_main {
                // We can now retrieve a pointer to the machine code.
                ptr = Some(self.module.get_finalized_function(id));
            }
        }

        if let Some(code) = ptr {
            return Ok(code);
        }

        Err("No Main".to_string())
    }

    /// Create a zero-initialized data section.
    pub fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {
        // The steps here are analogous to `compile`, except that data is much
        // simpler than functions.
        self.data_ctx.define(contents.into_boxed_slice());
        let id = self
            .module
            .declare_data(name, Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;

        self.module
            .define_data(id, &self.data_ctx)
            .map_err(|e| e.to_string())?;
        self.data_ctx.clear();
        self.module.finalize_definitions();
        let buffer = self.module.get_finalized_data(id);
        // TODO: Can we move the unsafe into cranelift?
        Ok(unsafe { slice::from_raw_parts(buffer.0, buffer.1) })
    }

    // Translate from AST nodes into Cranelift IR.
    fn translate(&mut self, params: Vec<String>, stmts: Expr) -> Result<(), String> {
        // The language currently only supports I64 values, though Cranelift
        // supports other types.
        let int = self.module.target_config().pointer_type();

        for _p in &params {
            self.ctx.func.signature.params.push(AbiParam::new(int));
        }

        // The language currently only supports one return value, though
        // Cranelift is designed to support more.
        self.ctx.func.signature.returns.push(AbiParam::new(int));

        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();

        // Since this is the entry block, add block parameters corresponding to
        // the function's parameters.
        builder.append_block_params_for_function_params(entry_block);

        // Tell the builder to emit code in this block.
        builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further
        // predecessors. Since it's the entry block, it won't have any
        // predecessors.
        builder.seal_block(entry_block);

        // The language allows variables to be declared implicitly.
        // Walk the AST and declare all implicitly-declared variables.
        let variables = declare_variables(int, &mut builder, &params, &stmts, entry_block);

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            int,
            builder,
            variables,
            module: &mut self.module,
            //current_block: None,
            current_block: Block::from_u32(0),
        };

        // the return value is the last Value in the code
        let return_value = trans.translate_expr(stmts);

        // for return value declared in function, use

        // let return_variable = trans.variables.get(&the_return).unwrap();
        // let return_value = trans.builder.use_var(*return_variable);

        // Emit the return instruction.
        trans.builder.ins().return_(&[return_value]);

        // Tell the builder we're done with this function.
        trans.builder.finalize();

        // print Cranelift IR. for debuging

        //let flags = settings::Flags::new(settings::builder());
        //let res = codegen::verify_function(&self.ctx.func, &flags);
        //println!("{}", &self.ctx.func.display(None));
        //if let Err(errors) = res {
        //    panic!("{}", errors);
        //}

        Ok(())
    }
}

/// A collection of state used for translating from AST nodes
/// into Cranelift IR.
struct FunctionTranslator<'a> {
    int: types::Type,
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut JITModule,

    // this is used for a simple break jump
    current_block: Block,
}

impl<'a> FunctionTranslator<'a> {
    /// When you write out instructions in Cranelift, you get back `Value`s. You
    /// can then use these references in other instructions.
    fn translate_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Int(int) => self.builder.ins().iconst(self.int, int),
            Expr::String(id) => self.translate_global_data_addr(get_string_id(id)),

            // simple optimizations for basic arithmetic
            Expr::Add(lhs, rhs) => match (*lhs, *rhs) {
                (Expr::Int(a), Expr::Int(b)) => self.builder.ins().iconst(self.int, a + b),
                (a, b) => {
                    let lhs = self.translate_expr(a);
                    let rhs = self.translate_expr(b);
                    self.builder.ins().iadd(lhs, rhs)
                }
            },

            Expr::Sub(lhs, rhs) => match (*lhs, *rhs) {
                (Expr::Int(a), Expr::Int(b)) => self.builder.ins().iconst(self.int, a - b),
                (a, b) => {
                    let lhs = self.translate_expr(a);
                    let rhs = self.translate_expr(b);
                    self.builder.ins().isub(lhs, rhs)
                }
            },

            Expr::Mul(lhs, rhs) => match (*lhs, *rhs) {
                (Expr::Int(a), Expr::Int(b)) => self.builder.ins().iconst(self.int, a * b),
                (a, b) => {
                    let lhs = self.translate_expr(a);
                    let rhs = self.translate_expr(b);
                    self.builder.ins().imul(lhs, rhs)
                }
            },

            Expr::Div(lhs, rhs) => match (*lhs, *rhs) {
                (Expr::Int(a), Expr::Int(b)) => self.builder.ins().iconst(self.int, a / b),
                (a, b) => {
                    let lhs = self.translate_expr(a);
                    let rhs = self.translate_expr(b);
                    self.builder.ins().udiv(lhs, rhs)
                }
            },

            Expr::Eq(lhs, rhs) => self.translate_icmp(IntCC::Equal, *lhs, *rhs),
            Expr::Ne(lhs, rhs) => self.translate_icmp(IntCC::NotEqual, *lhs, *rhs),
            Expr::Lt(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThan, *lhs, *rhs),
            Expr::Le(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThanOrEqual, *lhs, *rhs),
            Expr::Gt(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThan, *lhs, *rhs),
            Expr::Ge(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThanOrEqual, *lhs, *rhs),

            Expr::Call(name, args) => self.translate_call(name, args),
            Expr::Identifier(name) => {
                // `use_var` is used to read the value of a variable.
                let variable = self.variables.get(&name).expect("variable not defined");
                self.builder.use_var(*variable)
            }
            Expr::Assign(name, expr) => self.translate_assign(name, *expr),
            Expr::If(condition, then_body) => self.translate_if(*condition, *then_body),
            Expr::IfElse(condition, then_body, else_body) => {
                self.translate_if_else(*condition, *then_body, *else_body)
            }
            Expr::WhileLoop(condition, loop_body) => {
                self.translate_while_loop(*condition, *loop_body)
            }
            Expr::Field(exprs) => {
                let mut then_return = self.builder.ins().iconst(self.int, 0);

                for expr in exprs {
                    then_return = self.translate_expr(expr);
                }
                then_return
            }
            Expr::Break => {
                // self.builder.ins().jump(self.current_block, &[]);

                // kinda ugly ik, however I dont send extra data up the chain
                // so I cant do I jump because a block cannot have 2 jumps
                self.builder
                    .ins()
                    .brz(Value::from_u32(0), self.current_block, &[]);

                Value::from_u32(0)
            }
            Expr::Band(a, b) => {
                let lhs = self.translate_expr(*a);
                let rhs = self.translate_expr(*b);
                self.builder.ins().band(lhs, rhs)
            }
            Expr::Bor(a, b) => {
                let lhs = self.translate_expr(*a);
                let rhs = self.translate_expr(*b);
                self.builder.ins().bor(lhs, rhs)
            }
            Expr::Nxor(a, b) => {
                let lhs = self.translate_expr(*a);
                let rhs = self.translate_expr(*b);
                self.builder.ins().bxor(lhs, rhs)
            }
            Expr::Bnot(a) => {
                let lhs = self.translate_expr(*a);
                self.builder.ins().bnot(lhs)
            }
            Expr::Blshift(a, b) => {
                let lhs = self.translate_expr(*a);
                let rhs = self.translate_expr(*b);
                self.builder.ins().ishl(lhs, rhs)
            }
            Expr::Brshift(a, b) => {
                let lhs = self.translate_expr(*a);
                let rhs = self.translate_expr(*b);
                self.builder.ins().ushr(lhs, rhs)
            }
        }
    }

    fn translate_assign(&mut self, name: String, expr: Expr) -> Value {
        // `def_var` is used to write the value of a variable. Note that
        // variables can have multiple definitions. Cranelift will
        // convert them into SSA form for itself automatically.
        let new_value = self.translate_expr(expr);
        let variable = self.variables.get(&name).unwrap();
        self.builder.def_var(*variable, new_value);
        new_value
    }

    fn translate_icmp(&mut self, cmp: IntCC, lhs: Expr, rhs: Expr) -> Value {
        let lhs = self.translate_expr(lhs);
        let rhs = self.translate_expr(rhs);
        let c = self.builder.ins().icmp(cmp, lhs, rhs);

        self.builder.ins().bint(self.int, c)
    }

    fn translate_if(&mut self, condition: Expr, then_body: Expr) -> Value {
        // all if statments return 0 if the condition is not met, equvalent to if ... else 0
        // if ... else if ... will be translated to if ... else { if ... else 0 } as defined in grammar
        self.translate_if_else(condition, then_body, Expr::Int(0))
    }

    fn translate_if_else(&mut self, condition: Expr, then_body: Expr, else_body: Expr) -> Value {
        let condition_value = self.translate_expr(condition);

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        // If-else constructs in the language have a return value.
        self.builder.append_block_param(merge_block, self.int);

        // Test the if condition and conditionally branch.
        self.builder.ins().brz(condition_value, else_block, &[]);
        // Fall through to then block.
        self.builder.ins().jump(then_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_return = self.translate_expr(then_body);

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[then_return]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        let else_return = self.translate_expr(else_body);

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[else_return]);

        // Switch to the merge block for subsequent statements.
        self.builder.switch_to_block(merge_block);

        // We've now seen all the predecessors of the merge block.
        self.builder.seal_block(merge_block);

        // Read the value of the if-else by reading the merge block
        // parameter.
        let phi = self.builder.block_params(merge_block)[0];

        phi
    }

    fn translate_while_loop(&mut self, condition: Expr, loop_body: Expr) -> Value {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.current_block = exit_block;

        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_value = self.translate_expr(condition);

        self.builder.ins().brz(condition_value, exit_block, &[]);
        self.builder.ins().jump(body_block, &[]);

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);

        self.translate_expr(loop_body);

        self.builder.ins().jump(header_block, &[]);

        self.builder.switch_to_block(exit_block);

        // We've reached the bottom of the loop, so there will be no
        // more backedges to the header to exits to the bottom.
        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);

        // Just return 0 for now.
        self.builder.ins().iconst(self.int, 0)
    }

    fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        // Add a parameter for each argument.
        for _arg in &args {
            sig.params.push(AbiParam::new(self.int));
        }

        // For simplicity for now, just make all calls return a single I64.
        sig.returns.push(AbiParam::new(self.int));

        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self
            .module
            .declare_func_in_func(callee, &mut self.builder.func);

        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.push(self.translate_expr(arg))
        }
        let call = self.builder.ins().call(local_callee, &arg_values);
        self.builder.inst_results(call)[0]
    }

    fn translate_global_data_addr(&mut self, name: String) -> Value {
        let sym = self
            .module
            .declare_data(&name, Linkage::Export, true, false)
            .expect("problem declaring data object");
        let local_id = self
            .module
            .declare_data_in_func(sym, &mut self.builder.func);

        let pointer = self.module.target_config().pointer_type();
        self.builder.ins().symbol_value(pointer, local_id)
    }
}

fn declare_variables(
    int: types::Type,
    builder: &mut FunctionBuilder,
    params: &[String],
    stmts: &Expr,
    entry_block: Block,
) -> HashMap<String, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    for (i, name) in params.iter().enumerate() {
        let val = builder.block_params(entry_block)[i];
        let var = declare_variable(int, builder, &mut variables, &mut index, name);
        builder.def_var(var, val);
    }

    // as stmts is a Expr::Field, all the variables will be declared recursively
    // be aware that scope does not exist due to all variables in a function being
    // defined as 0 at the top
    declare_variables_in_stmt(int, builder, &mut variables, &mut index, stmts);

    variables
}

/// Recursively descend through the AST, translating all implicit
/// variable declarations.
fn declare_variables_in_stmt(
    int: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Variable>,
    index: &mut usize,
    expr: &Expr,
) {
    match *expr {
        Expr::Assign(ref name, ref to) => {
            declare_variable(int, builder, variables, index, name);
            declare_variables_in_stmt(int, builder, variables, index, &to);
        }
        Expr::Field(ref exprs) => {
            for stmt in exprs {
                declare_variables_in_stmt(int, builder, variables, index, &stmt);
            }
        }
        Expr::Call(_, ref args) => {
            for stmt in args {
                declare_variables_in_stmt(int, builder, variables, index, &stmt);
            }
        }
        Expr::IfElse(ref cond, ref then_body, ref else_body) => {
            declare_variables_in_stmt(int, builder, variables, index, &cond);
            declare_variables_in_stmt(int, builder, variables, index, &then_body);
            declare_variables_in_stmt(int, builder, variables, index, &else_body);
        }
        Expr::If(ref cond, ref then_body) => {
            declare_variables_in_stmt(int, builder, variables, index, &cond);
            declare_variables_in_stmt(int, builder, variables, index, &then_body);
        }
        Expr::WhileLoop(ref _condition, ref loop_body) => {
            declare_variables_in_stmt(int, builder, variables, index, &loop_body);
        }

        // this is due to variables with fields
        Expr::Add(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Sub(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Mul(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Div(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Eq(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Ne(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Lt(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Le(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Gt(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Ge(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }

        Expr::Bnot(ref a) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
        }

        Expr::Band(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Bor(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Nxor(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Blshift(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }
        Expr::Brshift(ref a, ref b) => {
            declare_variables_in_stmt(int, builder, variables, index, &a);
            declare_variables_in_stmt(int, builder, variables, index, &b);
        }

        _ => (),
    }
}

/// Declare a single variable declaration.
fn declare_variable(
    int: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Variable>,
    index: &mut usize,
    name: &str,
) -> Variable {
    let var = Variable::new(*index);
    if !variables.contains_key(name) {
        variables.insert(name.into(), var);
        builder.declare_var(var, int);
        *index += 1;
    }
    var
}
