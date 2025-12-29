// Azimuth Telemetry - Logs API
// 结构化日志记录

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
  attributes : Attributes
  trace_id : Array[Byte]?
  span_id : Array[Byte]?
  trace_flags : Byte?
  resource : Resource?
  instrumentation_scope : InstrumentationScope?
}

pub trait Logger {
  emit(self : Logger, log_record : LogRecord) -> Unit
  
  // 便捷方法
  debug(self : Logger, body : String, attributes? : Attributes) -> Unit
  info(self : Logger, body : String, attributes? : Attributes) -> Unit
  warn(self : Logger, body : String, attributes? : Attributes) -> Unit
  error(self : Logger, body : String, attributes? : Attributes) -> Unit
  fatal(self : Logger, body : String, attributes? : Attributes) -> Unit
}

pub trait LoggerProvider {
  get_logger(name : String, version? : String, schema_url? : String) -> Logger
}

// No-op实现
pub struct NoopLogger {
}

pub struct NoopLoggerProvider {
}

impl NoopLogger with Logger {
  emit(self : NoopLogger, log_record : LogRecord) -> Unit {
    // No-op implementation
  }
  
  debug(self : NoopLogger, body : String, attributes? : Attributes) -> Unit {
    // No-op implementation
  }
  
  info(self : NoopLogger, body : String, attributes? : Attributes) -> Unit {
    // No-op implementation
  }
  
  warn(self : NoopLogger, body : String, attributes? : Attributes) -> Unit {
    // No-op implementation
  }
  
  error(self : NoopLogger, body : String, attributes? : Attributes) -> Unit {
    // No-op implementation
  }
  
  fatal(self : NoopLogger, body : String, attributes? : Attributes) -> Unit {
    // No-op implementation
  }
}

impl NoopLoggerProvider with LoggerProvider {
  get_logger(self : NoopLoggerProvider, name : String, version? : String, schema_url? : String) -> Logger {
    NoopLogger::{}
  }
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
  attributes : Attributes
  trace_id : Array[Byte]?
  span_id : Array[Byte]?
  trace_flags : Byte?
  resource : Resource?
  instrumentation_scope : InstrumentationScope?
}

impl LogRecordBuilder {
  pub fn new() -> LogRecordBuilder {
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
  
  pub fn timestamp(self : LogRecordBuilder, timestamp : Int64) -> LogRecordBuilder {
    LogRecordBuilder::{ ..self, timestamp_unix_nanos: Some(timestamp) }
  }
  
  pub fn severity(self : LogRecordBuilder, severity : SeverityNumber) -> LogRecordBuilder {
    LogRecordBuilder::{ ..self, severity_number: Some(severity) }
  }
  
  pub fn body(self : LogRecordBuilder, body : String) -> LogRecordBuilder {
    LogRecordBuilder::{ ..self, body: Some(body) }
  }
  
  pub fn with_attribute(self : LogRecordBuilder, key : String, value : AttributeValue) -> LogRecordBuilder {
    let new_attributes = self.attributes.push((key, value))
    LogRecordBuilder::{ ..self, attributes: new_attributes }
  }
  
  pub fn build(self : LogRecordBuilder) -> LogRecord {
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
}