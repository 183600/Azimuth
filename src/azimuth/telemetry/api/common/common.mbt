// Azimuth Telemetry - Common API Types
// 公共类型定义，用于所有Telemetry信号

pub enum AttributeValue {
  StringValue(String)
  IntValue(Int64)
  FloatValue(Double)
  BoolValue(Bool)
  ArrayStringValue(Array[String])
  ArrayIntValue(Array[Int64])
  ArrayFloatValue(Array[Double])
  ArrayBoolValue(Array[Bool])
}

pub type Attributes = Array[(String, AttributeValue)]

pub struct Resource {
  service_name : String
  service_version : String?
  telemetry_sdk_name : String
  telemetry_sdk_version : String
  attributes : Attributes
}

pub struct InstrumentationScope {
  name : String
  version : String?
  schema_url : String?
}

pub fn Resource::default(service_name : String) -> Resource {
  Resource::{
    service_name,
    service_version: None,
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: []
  }
}

pub fn AttributeValue::string(value : String) -> AttributeValue {
  StringValue(value)
}

pub fn AttributeValue::int(value : Int64) -> AttributeValue {
  IntValue(value)
}

pub fn AttributeValue::float(value : Double) -> AttributeValue {
  FloatValue(value)
}

pub fn AttributeValue::bool(value : Bool) -> AttributeValue {
  BoolValue(value)
}

pub fn AttributeValue::array_string(value : Array[String]) -> AttributeValue {
  ArrayStringValue(value)
}

pub fn AttributeValue::array_int(value : Array[Int64]) -> AttributeValue {
  ArrayIntValue(value)
}

pub fn AttributeValue::array_float(value : Array[Double]) -> AttributeValue {
  ArrayFloatValue(value)
}

pub fn AttributeValue::array_bool(value : Array[Bool]) -> AttributeValue {
  ArrayBoolValue(value)
}