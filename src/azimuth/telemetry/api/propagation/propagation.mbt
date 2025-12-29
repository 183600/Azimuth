// Azimuth Telemetry - Propagation API
// 跨进程上下文传播

pub trait TextMapCarrier {
  get(self : TextMapCarrier, key : String) -> String?
  set(self : TextMapCarrier, key : String, value : String) -> Unit
  keys(self : TextMapCarrier) -> Array[String]
}

pub trait TextMapPropagator {
  inject(self : TextMapPropagator, ctx : Context, carrier : TextMapCarrier) -> Unit
  extract(self : TextMapPropagator, ctx : Context, carrier : TextMapCarrier) -> Context
}

// W3C TraceContext Propagator
pub struct W3CTraceContextPropagator {
}

pub struct TraceContextHeader {
  version : Byte
  trace_id : String  // 32 hex chars
  span_id : String   // 16 hex chars
  trace_flags : Byte
}

pub const TRACE_PARENT_HEADER : String = "traceparent"
pub const TRACE_STATE_HEADER : String = "tracestate"

impl W3CTraceContextPropagator with TextMapPropagator {
  inject(self : W3CTraceContextPropagator, ctx : Context, carrier : TextMapCarrier) -> Unit {
    // 简化实现：在实际实现中需要从Context中提取SpanContext
    let trace_parent = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
    carrier.set(TRACE_PARENT_HEADER, trace_parent)
  }
  
  extract(self : W3CTraceContextPropagator, ctx : Context, carrier : TextMapCarrier) -> Context {
    match carrier.get(TRACE_PARENT_HEADER) {
      Some(_trace_parent) => {
        // 简化实现：在实际实现中需要解析traceparent并更新Context
        ctx
      }
      None => ctx
    }
  }
}

// W3C Baggage Propagator
pub struct W3CBaggagePropagator {
}

pub const BAGGAGE_HEADER : String = "baggage"

impl W3CBaggagePropagator with TextMapPropagator {
  inject(self : W3CBaggagePropagator, ctx : Context, carrier : TextMapCarrier) -> Unit {
    // 简化实现：在实际实现中需要从Context中提取Baggage
    let baggage_value = "key1=value1,key2=value2"
    carrier.set(BAGGAGE_HEADER, baggage_value)
  }
  
  extract(self : W3CBaggagePropagator, ctx : Context, carrier : TextMapCarrier) -> Context {
    match carrier.get(BAGGAGE_HEADER) {
      Some(_baggage_header) => {
        // 简化实现：在实际实现中需要解析baggage header并更新Context
        ctx
      }
      None => ctx
    }
  }
}

// Composite Propagator
pub struct CompositePropagator {
  propagators : Array[TextMapPropagator]
}

impl CompositePropagator {
  pub fn new(propagators : Array[TextMapPropagator]) -> CompositePropagator {
    CompositePropagator::{ propagators }
  }
}

impl CompositePropagator with TextMapPropagator {
  inject(self : CompositePropagator, ctx : Context, carrier : TextMapCarrier) -> Unit {
    for propagator in self.propagators {
      propagator.inject(ctx, carrier)
    }
  }
  
  extract(self : CompositePropagator, ctx : Context, carrier : TextMapCarrier) -> Context {
    let mut current_ctx = ctx
    for propagator in self.propagators {
      current_ctx = propagator.extract(current_ctx, carrier)
    }
    current_ctx
  }
}

// Map-based TextMapCarrier implementation
pub struct MapCarrier {
  data : Array[(String, String)]
}

impl MapCarrier {
  pub fn new() -> MapCarrier {
    MapCarrier::{ data: [] }
  }
  
  pub fn from_map(data : Array[(String, String)]) -> MapCarrier {
    MapCarrier::{ data }
  }
  
  pub fn to_map(self : MapCarrier) -> Array[(String, String)] {
    self.data
  }
}

impl MapCarrier with TextMapCarrier {
  get(self : MapCarrier, key : String) -> String? {
    for (k, v) in self.data {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  set(self : MapCarrier, key : String, value : String) -> Unit {
    // 注意：这里是简化实现，实际应该返回新的MapCarrier
    // 因为MoonBit的Array是不可变的
  }
  
  keys(self : MapCarrier) -> Array[String] {
    let mut keys = []
    for (k, _) in self.data {
      keys = keys.push(k)
    }
    keys
  }
}