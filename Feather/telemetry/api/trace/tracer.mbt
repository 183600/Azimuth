// telemetry/api/trace/tracer.mbt

// Tracer is the interface for creating spans
pub trait Tracer {
  // Start a new span
  fn start_span(
    self,
    name : String,
    kind? : SpanKind,
    attributes? : @common.Attributes,
    links? : Array[Link],
    start_time? : Int64,
    context? : @context.Context,
  ) -> (@context.Context, Span)
  
  // Get the instrumentation scope
  fn instrumentation_scope(self) -> @common.InstrumentationScope
}

// TracerProvider is the interface for getting tracers
pub trait TracerProvider {
  // Get a tracer with the given name and optional version and schema URL
  fn get_tracer(
    self, 
    name : String, 
    version? : String, 
    schema_url? : String
  ) -> Tracer
}

// No-op implementation of Tracer
pub type NoopTracer {
  scope : @common.InstrumentationScope
}

impl Tracer for NoopTracer {
  fn start_span(
    self,
    name : String,
    kind? : SpanKind,
    attributes? : @common.Attributes,
    links? : Array[Link],
    start_time? : Int64,
    context? : @context.Context,
  ) -> (@context.Context, Span) {
    let ctx = context.unwrap_or(@context::empty())
    let span_ctx = {
      trace_id: Array::make(16, 0_byte),
      span_id: Array::make(8, 0_byte),
      trace_flags: 0_byte,
      trace_state: "",
      is_remote: false
    }
    let span = NoopSpan::new(span_ctx)
    (ctx, span)
  }
  
  fn instrumentation_scope(self) -> @common.InstrumentationScope {
    self.scope
  }
}

// No-op implementation of TracerProvider
pub type NoopTracerProvider {}

impl TracerProvider for NoopTracerProvider {
  fn get_tracer(
    self, 
    name : String, 
    version? : String, 
    schema_url? : String
  ) -> Tracer {
    let scope = {
      name: name,
      version: version.unwrap_or(""),
      schema_url: schema_url.unwrap_or("")
    }
    NoopTracer::new(scope)
  }
}