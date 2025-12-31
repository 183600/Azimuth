// Azimuth Telemetry - Logs API
// 结构化日志记录

use azimuth.telemetry.api.common
use azimuth.telemetry.api.context

pub enum SeverityNumber {
  Trace
  Debug
  Info
  Warn
  Error
  Fatal
}

pub struct LogRecord {
  timestamp_unix_nanos : Int64
  observed_timestamp_unix_nanos : Int64?
  severity_number : SeverityNumber
  severity_text : String?
  body : String?
  attributes : Array[(String, AttributeValue)]
  trace_id : Array[Byte]?
  span_id : Array[Byte]?
  trace_flags : Byte?
  resource : Resource?
  instrumentation_scope : InstrumentationScope?
}

pub trait Logger {
  emit(log_record : LogRecord) -> Unit
  
  // 便捷方法
  debug(body : String, attributes? : Array[(String, AttributeValue)]) -> Unit
  info(body : String, attributes? : Array[(String, AttributeValue)]) -> Unit
  warn(body : String, attributes? : Array[(String, AttributeValue)]) -> Unit
  error(body : String, attributes? : Array[(String, AttributeValue)]) -> Unit
  fatal(body : String, attributes? : Array[(String, AttributeValue)]) -> Unit
}

pub trait LoggerProvider {
  get_logger(name : String, version? : String, schema_url? : String) -> NoopLogger
}

// No-op实现
pub struct NoopLogger {
}

pub struct NoopLoggerProvider {
}

pub fn NoopLogger::emit(log_record : LogRecord) -> Unit {
  // No-op implementation
}

pub fn NoopLogger::debug(body : String, attributes? : Array[(String, AttributeValue)]) -> Unit {
  // No-op implementation
}

pub fn NoopLogger::info(body : String, attributes? : Array[(String, AttributeValue)]) -> Unit {
  // No-op implementation
}

pub fn NoopLogger::warn(body : String, attributes? : Array[(String, AttributeValue)]) -> Unit {
  // No-op implementation
}

pub fn NoopLogger::error(body : String, attributes? : Array[(String, AttributeValue)]) -> Unit {
  // No-op implementation
}

pub fn NoopLogger::fatal(body : String, attributes? : Array[(String, AttributeValue)]) -> Unit {
  // No-op implementation
}

pub fn NoopLoggerProvider::get_logger(name : String, version? : String, schema_url? : String) -> NoopLogger {
  NoopLogger::{}
}

// 辅助函数
pub fn LogRecord::builder() -> LogRecordBuilder {
  LogRecordBuilder::new()
}

pub struct LogRecordBuilder {
  timestamp_unix_nanos : Int64?
  observed_timestamp_unix_nanos : Int64?
  severity_number : SeverityNumber?
  severity_text : String?
  body : String?
  attributes : Array[(String, AttributeValue)]
  trace_id : Array[Byte]?
  span_id : Array[Byte]?
  trace_flags : Byte?
  resource : Resource?
  instrumentation_scope : InstrumentationScope?
}

pub fn LogRecordBuilder::new() -> LogRecordBuilder {
  LogRecordBuilder::{
    timestamp_unix_nanos: None,
    observed_timestamp_unix_nanos: None,
    severity_number: None,
    severity_text: None,
    body: None,
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
}

pub fn LogRecordBuilder::timestamp(self : LogRecordBuilder, timestamp : Int64) -> LogRecordBuilder {
  LogRecordBuilder::{ ..self, timestamp_unix_nanos: Some(timestamp) }
}

pub fn LogRecordBuilder::severity(self : LogRecordBuilder, severity : SeverityNumber) -> LogRecordBuilder {
  LogRecordBuilder::{ ..self, severity_number: Some(severity) }
}

pub fn LogRecordBuilder::body(self : LogRecordBuilder, body : String) -> LogRecordBuilder {
  LogRecordBuilder::{ ..self, body: Some(body) }
}

pub fn LogRecordBuilder::with_attribute(self : LogRecordBuilder, key : String, value : AttributeValue) -> LogRecordBuilder {
  let new_attributes = []
  let mut i = 0
  while i < self.attributes.length() {
    new_attributes.push(self.attributes[i])
    i = i + 1
  }
  new_attributes.push((key, value))
  LogRecordBuilder::{ ..self, attributes: new_attributes }
}

pub fn LogRecordBuilder::build(self : LogRecordBuilder) -> LogRecord {
  LogRecord::{
    timestamp_unix_nanos: self.timestamp_unix_nanos.unwrap_or(0L),
    observed_timestamp_unix_nanos: self.observed_timestamp_unix_nanos,
    severity_number: self.severity_number.unwrap_or(Info),
    severity_text: self.severity_text,
    body: self.body,
    attributes: self.attributes,
    trace_id: self.trace_id,
    span_id: self.span_id,
    trace_flags: self.trace_flags,
    resource: self.resource,
    instrumentation_scope: self.instrumentation_scope
  }
}