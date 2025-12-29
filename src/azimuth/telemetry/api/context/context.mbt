// Azimuth Telemetry - Context API
// 上下文传播和管理

pub struct ContextKey {
  name : String
}

pub struct Context {
  values : Array[(String, String)]  // 简化实现，使用字符串存储
}

pub struct Baggage {
  entries : Array[(String, String)]
}

// Context相关函数
pub fn Context::empty() -> Context {
  Context::{ values: [] }
}

pub fn Context::with_value(self : Context, key : ContextKey, value : String) -> Context {
  // 简化实现，将值转换为字符串
  let new_values = []
  let mut i = 0
  while i < self.values.length() {
    new_values.push(self.values[i])
    i = i + 1
  }
  new_values.push((key.name, value))
  Context::{ values: new_values }
}

pub fn Context::get(self : Context, key : ContextKey) -> String? {
  let mut index = 0
  while index < self.values.length() {
    let (k, v) = self.values[index]
    if k == key.name {
      return Some(v)
    }
    index = index + 1
  }
  None
}

// Baggage相关函数
pub fn Baggage::empty() -> Baggage {
  Baggage::{ entries: [] }
}

pub fn Baggage::with_entry(self : Baggage, key : String, value : String) -> Baggage {
  let new_entries = []
  let mut i = 0
  while i < self.entries.length() {
    new_entries.push(self.entries[i])
    i = i + 1
  }
  new_entries.push((key, value))
  Baggage::{ entries: new_entries }
}

pub fn Baggage::get(self : Baggage, key : String) -> String? {
  let mut index = 0
  while index < self.entries.length() {
    let (k, v) = self.entries[index]
    if k == key {
      return Some(v)
    }
    index = index + 1
  }
  None
}

// 创建ContextKey的便捷函数
pub fn create_key(name : String) -> ContextKey {
  ContextKey::{ name }
}