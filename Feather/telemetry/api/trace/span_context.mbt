// telemetry/api/trace/span_context.mbt

// SpanContext represents the context of a span
pub type SpanContext {
  trace_id : Array[Byte]  // 16 bytes
  span_id : Array[Byte]   // 8 bytes  
  trace_flags : Byte      // 8-bit flags
  trace_state : String    // tracestate header value
  is_remote : Bool        // whether this context was propagated from a remote parent
}

// SpanKind represents the type of span
pub enum SpanKind {
  Internal
  Server
  Client
  Producer
  Consumer
}

// StatusCode represents the status code of a span
pub enum StatusCode {
  Unset
  Ok
  Error
}

// Status represents the status of a span
pub type Status {
  code : StatusCode
  description : String
}

// Event represents a timed event within a span
pub type Event {
  name : String
  attributes : @common.Attributes
  time_unix_nanos : Int64
}

// Link represents a link to another span
pub type Link {
  span_context : SpanContext
  attributes : @common.Attributes
}