pub mod instructions;

use std::{cell::RefCell, collections::HashMap, marker::PhantomData, rc::Rc};

use crate::{assembler::instructions::{Instruction, Register}, context::{Context, Node, NodeId}, error::{ErrorKind, FormattedError}, lex::{Number, Token}, preprocess::PreProcessor};

pub struct Assembler<'a> {
    context: Rc<RefCell<Context<'a>>>,
    preprocessor: PreProcessor<'a>,

    peek: Option<Node<Token<'a>>>,
    current_section: &'a str,
    sections: HashMap<&'a str, Section<'a>>,

    labels: HashMap<&'a str, Label<'a>>
}

pub struct Label<'a>{
    source: NodeId,
    section: &'a str,
    offset: u64,
    size: u64,
}

pub struct Section<'a>{
    name: &'a str,
    start: u64,
    size: u64,
    instructions: Vec<Instruction<'a>>
}

pub enum Argument<'a>{
    Number(Number<'a>),
    Ident(&'a str),
    Register(Register),
    Expression,
}




impl<'a> Assembler<'a> {
    pub fn new(context: Rc<RefCell<Context<'a>>>, preprocessor: PreProcessor<'a>) -> Self {
        Self {
            context,
            preprocessor,
            current_section: "text",
            sections: HashMap::new(),
            labels: HashMap::new(),
            peek: None,
        }
    }

    pub fn assemble(&mut self, path: impl Into<String>) -> Vec<u8> {
        self.preprocessor.begin(path);
        self.sections.insert("text", Section { name: "text", start: 0x4000, instructions: Vec::new(), size: 0 });
        
        while let Some(Node(Token::NewLine, _)) = self.peek(){
            self.next();
        } 
        while self.peek().is_some(){
            self.assemble_line();
            while let Some(Node(Token::NewLine, _)) = self.peek(){
                self.next();
            } 
        }

        Vec::new()
    }

    fn next(&mut self) -> Option<Node<Token<'a>>>{
        self.peek.take().or_else(|| self.preprocessor.next())
    }

    fn peek(&mut self) -> Option<Node<Token<'a>>>{
        if self.peek.is_none(){
            self.peek = self.preprocessor.next();
        }
        self.peek
    }

    fn assemble_line(&mut self){
        match self.next(){
            Some(Node(Token::Ident(ident), n)) => {
                match ident{
                    "lui" => {}
                    "auipc" => {}
                    "jal" => {}
                    "jalr" => {}
                    
                    "beq" => {}
                    "bne" => {}
                    "blt" => {}
                    "bge" => {}
                    "bltu" => {}
                    "bgeu" => {}
                    
                    "lb" => {}
                    "lh" => {}
                    "lw" => {}
                    "lbu" => {}
                    "lhu" => {}

                    "sb" => {}
                    "sh" => {}
                    "sw" => {}

                    _ => self.context.borrow_mut().report_error(Node(format!("Unrecognized mnemonic '{ident}'"), n)),
                }
                loop{
                    match self.peek(){
                        None | Some(Node(Token::NewLine, _)) => break,
                        Some(unexpected) => 
                        self.context.borrow_mut().report_error(unexpected.map(|t| 
                            format!("Unexpected token '{t:?}' at end of line"))),
                    }   
                    self.next();
                }
            }
            Some(Node(Token::Label(label), source)) => {
                if let Some(previous) = self.labels.get(label){
                    self.context.borrow_mut().report(|ctx|{
                        FormattedError::default()
                            .add(ctx, source, ErrorKind::Error, "Label bound more than once")
                            .add(ctx, previous.source, ErrorKind::Info, "First bound here")
                    });
                    return;
                }
                self.labels.insert(label, Label { 
                    source,
                    section: self.current_section, 
                    offset: self.sections.get(self.current_section).map(|s| s.size).unwrap_or(0),
                    size: 0
                });
            }
            Some(unexpected) => 
                self.context.borrow_mut().report_error(unexpected.map(|t| format!("Unexpected token {t:?} expected identifier or label"))),
            None => {}
        }
    }
}
