#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogOn {
    Error,
    Warning,
    None,
}

impl LogOn {
    pub fn supress(self, supress: bool) -> Self {
        if supress { LogOn::None } else { self }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ImplicitCastConfig {
    pub narrowing: LogOn,
    pub widening: LogOn,
    pub sign: LogOn,
    pub lossy: LogOn,
    pub f2i: LogOn,
    pub i2f: LogOn,
}

impl ImplicitCastConfig {
    pub fn supress(
        self,
        narrowing: bool,
        widening: bool,
        sign: bool,
        lossy: bool,
        f2i: bool,
        i2f: bool,
    ) -> Self {
        Self {
            narrowing: self.narrowing.supress(narrowing),
            widening: self.widening.supress(widening),
            sign: self.sign.supress(sign),
            lossy: self.lossy.supress(lossy),
            f2i: self.f2i.supress(f2i),
            i2f: self.i2f.supress(i2f),
        }
    }
}

#[derive(Debug)]
pub struct AssemblerConfig {
    pub producer_stack_limit: usize,
    pub filter_stack_limit: usize,
    pub pic: bool,

    pub implicit_cast_defaults: ImplicitCastConfig,
    pub implicit_cast_label_offset: ImplicitCastConfig,
    pub implicit_cast_shift_value: ImplicitCastConfig,
}

impl AssemblerConfig {
    pub fn new() -> Self {
        Self {
            producer_stack_limit: 100,
            filter_stack_limit: 100,
            pic: true,
            implicit_cast_defaults: ImplicitCastConfig {
                narrowing: LogOn::Error,
                widening: LogOn::Error,
                sign: LogOn::Error,
                lossy: LogOn::Error,
                f2i: LogOn::Error,
                i2f: LogOn::Error,
            },
            implicit_cast_label_offset: ImplicitCastConfig {
                narrowing: LogOn::Warning,
                widening: LogOn::Warning,
                sign: LogOn::None,
                lossy: LogOn::Warning,
                f2i: LogOn::Error,
                i2f: LogOn::Error,
            },
            implicit_cast_shift_value: ImplicitCastConfig {
                narrowing: LogOn::None,
                widening: LogOn::None,
                sign: LogOn::None,
                lossy: LogOn::None,
                f2i: LogOn::Error,
                i2f: LogOn::Error,
            },
        }
    }
}
