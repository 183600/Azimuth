// Azimuth Telemetry - Trace API
// 分布式链路追踪

pub enum SpanKind {
  Internal
  Server
  Client
  Producer
  Consumer
}

pub enum StatusCode {
  Unset
  Ok
  Error
}

pub struct SpanContext {
  trace_id : Array[Byte]  // 16 bytes
  span_id : Array[Byte]   // 8 bytes
  trace_flags : Byte
  trace_state : String
}

pub struct Span {
  name : String
  context : SpanContext
  kind : SpanKind
  parent_span_id : Array[Byte]?
  start_time_unix_nanos : Int64
  end_time_unix_nanos : Int64?
  status : StatusCode
  status_description : String?
  attributes : Array[(String, common.AttributeValue)]
  events : Array[SpanEvent]
  links : Array[SpanLink]
}

pub struct SpanEvent {
  name : String
  timestamp_unix_nanos : Int64
  attributes : Array[(String, common.AttributeValue)]
}

pub struct SpanLink {
  context : SpanContext
  attributes : Array[(String, common.AttributeValue)]
}

pub trait Tracer {
  start_span(
    ctx : context.Context,
    name : String,
    kind? : SpanKind,
    attributes? : Array[(String, common.AttributeValue)],
    start_time_unix_nanos? : Int64
  ) -> (context.Context, Span)
}

pub trait TracerProvider {
  get_tracer(name : String, version? : String) -> Tracer
}

// No-op实现（默认实现）
pub struct NoopTracer {
}

pub struct NoopTracerProvider {
}

pub fn NoopTracer::start_span(
  ctx : context.Context,
  name : String,
  kind? : SpanKind,
  attributes? : Array[(String, common.AttributeValue)],
  start_time_unix_nanos? : Int64
) -> (context.Context, Span) {
  let span_context = SpanContext::{
    trace_id: Array::make(16, 0_byte),
    span_id: Array::make(8, 0_byte),
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let span = Span::{
    name,
    context: span_context,
    kind: kind.unwrap_or(Internal),
    parent_span_id: None,
    start_time_unix_nanos: start_time_unix_nanos.unwrap_or(0L),
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: attributes.unwrap_or([]),
    events: [],
    links: []
  }
  
  (ctx, span)
}

pub fn NoopTracerProvider::get_tracer(name : String, version? : String) -> Tracer {
  NoopTracer::{}
}