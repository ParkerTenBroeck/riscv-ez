use bumpalo::Bump;

pub fn main() {
    use assembler::context::{Context};
    use assembler::{assembler::Assembler, preprocess::PreProcessor};
    use std::{cell::RefCell, rc::Rc};

    let mut bump = Bump::new();
    let context = Rc::new(RefCell::new(Context::new(&mut bump, |path, _ctx| Ok(std::fs::read_to_string(path)?))));
    let preprocessor = PreProcessor::new(context.clone());
    let mut assember = Assembler::new(context.clone(), preprocessor);

    assember.assemble("test.asm");
    context.borrow().print_errors();
}
