// telemetry/api/trace/span.mbt

// Span represents a single operation within a trace
pub trait Span {
  // Get the span context
  fn span_context(self) -> SpanContext
  
  // Check if the span is recording
  fn is_recording(self) -> Bool
  
  // Set an attribute on the span
  fn set_attribute(self, key : String, value : @common.AttributeValue) -> Unit
  
  // Add an event to the span
  fn add_event(self, name : String, attributes? : @common.Attributes, timestamp? : Int64) -> Unit
  
  // Add a link to another span
  fn add_link(self, link : Link) -> Unit
  
  // Set the status of the span
  fn set_status(self, code : StatusCode, description? : String) -> Unit
  
  // Record an exception on the span
  fn record_exception(self, exception : String, attributes? : @common.Attributes, timestamp? : Int64) -> Unit
  
  // Update the name of the span
  fn update_name(self, name : String) -> Unit
  
  // End the span
  fn end(self, end_time? : Int64) -> Unit
}

// No-op implementation of Span for when tracing is disabled
pub type NoopSpan {
  ctx : SpanContext
}

impl Span for NoopSpan {
  fn span_context(self) -> SpanContext {
    self.ctx
  }
  
  fn is_recording(self) -> Bool {
    false
  }
  
  fn set_attribute(self, key : String, value : @common.AttributeValue) -> Unit {
    // No-op
  }
  
  fn add_event(self, name : String, attributes? : @common.Attributes, timestamp? : Int64) -> Unit {
    // No-op
  }
  
  fn add_link(self, link : Link) -> Unit {
    // No-op
  }
  
  fn set_status(self, code : StatusCode, description? : String) -> Unit {
    // No-op
  }
  
  fn record_exception(self, exception : String, attributes? : @common.Attributes, timestamp? : Int64) -> Unit {
    // No-op
  }
  
  fn update_name(self, name : String) -> Unit {
    // No-op
  }
  
  fn end(self, end_time? : Int64) -> Unit {
    // No-op
  }
}