pub fn main() {
    use assembler::context::{Context, SourceStorage};
    use assembler::{assembler::Assembler, preprocess::PreProcessor};
    use std::{cell::RefCell, rc::Rc};

    let mut storage = SourceStorage::new(|path, _ctx| Ok(std::fs::read_to_string(path)?));
    let context = Rc::new(RefCell::new(Context::new(&mut storage)));
    let preprocessor = PreProcessor::new(context.clone());
    let mut assember = Assembler::new(context.clone(), preprocessor);

    assember.assemble("test.asm");
    context.borrow().print_errors();
}
