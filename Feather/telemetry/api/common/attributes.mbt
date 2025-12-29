// telemetry/api/common/attributes.mbt

// AttributeValue represents different types of values that can be stored in attributes
pub enum AttributeValue {
  String(String)
  Int64(Int64)
  Float64(Float64)
  Bool(Bool)
  ArrayString(Array[String])
  ArrayInt64(Array[Int64])
  ArrayFloat64(Array[Float64])
  ArrayBool(Array[Bool])
}

// Attributes represents a collection of key-value pairs for telemetry data
pub type Attributes Array[(String, AttributeValue)]

// Resource represents entity producing telemetry data
pub type Resource {
  attributes : Attributes
}

// InstrumentationScope represents the instrumentation library/scope
pub type InstrumentationScope {
  name : String
  version : String
  schema_url : String
}