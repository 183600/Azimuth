// telemetry/api/context/context.mbt

// ContextKey represents a typed key for storing values in Context
pub type ContextKey[T] {
  key : String
}

// Context represents an immutable context for carrying telemetry data
pub type Context {
  // Internal representation - using a simple map for now
  // In a real implementation, this would be more sophisticated
  values : Map[String, (String, Any)] // (type_name, value)
}

// Create a new empty context
pub fn empty() -> Context {
  { values: Map::new() }
}

// Set a value in the context
pub fn with_value[T](ctx : Context, key : ContextKey[T], value : T) -> Context {
  let new_values = ctx.values.insert(key.key, (type_string[T], value))
  { values: new_values }
}

// Get a value from the context
pub fn get[T](ctx : Context, key : ContextKey[T]) -> Option[T] {
  match ctx.values.get(key.key) {
    Some((type_str, value)) => {
      if type_str == type_string[T] {
        value as T
      } else {
        None
      }
    }
    None => None
  }
}

// Helper function to get type string (simplified)
fn type_string[T]() -> String {
  // This is a simplified implementation
  // In a real implementation, this would use reflection or compile-time type info
  match type_id[T] {
    0 => "String"
    1 => "Int64" 
    2 => "Float64"
    3 => "Bool"
    _ => "Unknown"
  }
}

// Type ID mapping (simplified)
fn type_id[T]() -> Int {
  // This is a placeholder - real implementation would need proper type introspection
  0
}