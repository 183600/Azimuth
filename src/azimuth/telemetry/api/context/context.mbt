// Azimuth Telemetry - Context API
// 上下文传播和管理

pub struct ContextKey[T] {
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

pub fn[T] Context::with_value(self : Context, key : ContextKey[T], value : T) -> Context {
  // 简化实现，将值转换为字符串
  let value_str = value.to_string()
  let new_values = self.values.push((key.name, value_str))
  Context::{ values: new_values }
}

pub fn[T] Context::get(self : Context, key : ContextKey[T]) -> String? {
  for (k, v) in self.values {
    if k == key.name {
      return Some(v)
    }
  }
  None
}

// Baggage相关函数
pub fn Baggage::empty() -> Baggage {
  Baggage::{ entries: [] }
}

pub fn Baggage::with_entry(self : Baggage, key : String, value : String) -> Baggage {
  let new_entries = self.entries.push((key, value))
  Baggage::{ entries: new_entries }
}

pub fn Baggage::get(self : Baggage, key : String) -> String? {
  for (k, v) in self.entries {
    if k == key {
      return Some(v)
    }
  }
  None
}

// 创建ContextKey的便捷函数
pub fn[T] create_key(name : String) -> ContextKey[T] {
  ContextKey::{ name }
}