use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Default)]
pub struct EnvNode {
    parent: Option<Env>,
    vars: HashMap<String, Object>,
}

pub type Env = Rc<RefCell<EnvNode>>;

impl EnvNode {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn extend(parent: Env) -> Env {
        Rc::new(RefCell::new(EnvNode {
            vars: HashMap::new(),
            parent: Some(parent),
        }))
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.vars.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().get(name))
        })
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.vars.insert(name.to_string(), val);
        dbg!(&self);
    }
}

pub fn new_env() -> Env {
    Rc::new(RefCell::new(EnvNode::new()))
}

pub fn extend_env(parent: Env) -> Env {
    EnvNode::extend(parent)
}
