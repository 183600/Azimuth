// telemetry/api/propagation/propagator.mbt

// TextMapCarrier is the interface for extracting and injecting values
pub trait TextMapCarrier {
  // Get a value by key
  fn get(self, key : String) -> Option[String]
  
  // Set a key-value pair
  fn set(self, key : String, value : String) -> Unit
  
  // Get all keys
  fn keys(self) -> Array[String]
}

// TextMapPropagator is the interface for context propagation
pub trait TextMapPropagator {
  // Inject context into carrier
  fn inject(self, ctx : @context.Context, carrier : TextMapCarrier) -> Unit
  
  // Extract context from carrier
  fn extract(self, ctx : @context.Context, carrier : TextMapCarrier) -> @context.Context
  
  // Get the fields this propagator injects
  fn fields(self) -> Array[String]
}

// CompositePropagator composes multiple propagators
pub type CompositePropagator {
  propagators : Array[TextMapPropagator]
}

impl TextMapPropagator for CompositePropagator {
  fn inject(self, ctx : @context.Context, carrier : TextMapCarrier) -> Unit {
    for propagator in self.propagators {
      propagator.inject(ctx, carrier)
    }
  }
  
  fn extract(self, ctx : @context.Context, carrier : TextMapCarrier) -> @context.Context {
    let mut current_ctx = ctx
    for propagator in self.propagators {
      current_ctx = propagator.extract(current_ctx, carrier)
    }
    current_ctx
  }
  
  fn fields(self) -> Array[String] {
    let mut all_fields = Array::new()
    for propagator in self.propagators {
      all_fields = all_fields.concat(propagator.fields())
    }
    all_fields
  }
}

// HttpHeadersCarrier implements TextMapCarrier for HTTP headers
pub type HttpHeadersCarrier {
  headers : Map[String, String]
}

impl TextMapCarrier for HttpHeadersCarrier {
  fn get(self, key : String) -> Option[String] {
    self.headers.get(key)
  }
  
  fn set(self, key : String, value : String) -> Unit {
    // Note: This would need to be mutable in a real implementation
    // For now, this is a simplified version
  }
  
  fn keys(self) -> Array[String] {
    self.headers.keys()
  }
}