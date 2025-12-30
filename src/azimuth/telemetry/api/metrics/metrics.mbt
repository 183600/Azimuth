// Azimuth Telemetry - Metrics API
// 指标收集和聚合



pub enum InstrumentType {
  Counter
  Histogram
  UpDownCounter
  Gauge
  ObservableCounter
  ObservableGauge
  ObservableUpDownCounter
}

pub struct Measurement {
  value : Double
  attributes : Array[(String, common::AttributeValue)]
}

pub trait Counter {
  add(amount : Int64, attributes? : Array[(String, common::AttributeValue)]) -> Unit
}

pub trait Histogram {
  record(value : Double, attributes? : Array[(String, AttributeValue)]) -> Unit
}

pub trait UpDownCounter {
  add(amount : Int64, attributes? : Array[(String, AttributeValue)]) -> Unit
}

pub trait Gauge {
  record(value : Double, attributes? : Array[(String, AttributeValue)]) -> Unit
}

pub trait Meter {
  create_counter(name : String, unit? : String, description? : String) -> Counter
  create_histogram(name : String, unit? : String, description? : String) -> Histogram
  create_up_down_counter(name : String, unit? : String, description? : String) -> UpDownCounter
  create_gauge(name : String, unit? : String, description? : String) -> Gauge
}

pub trait MeterProvider {
  get_meter(name : String, version? : String, schema_url? : String) -> Meter
}

// No-op实现
pub struct NoopCounter {
}

pub struct NoopHistogram {
}

pub struct NoopUpDownCounter {
}

pub struct NoopGauge {
}

pub struct NoopMeter {
}

pub struct NoopMeterProvider {
}

pub fn NoopCounter::add(amount : Int64, attributes? : Array[(String, AttributeValue)]) -> Unit {
  // No-op implementation
}

pub fn NoopHistogram::record(value : Double, attributes? : Array[(String, AttributeValue)]) -> Unit {
  // No-op implementation
}

pub fn NoopUpDownCounter::add(amount : Int64, attributes? : Array[(String, AttributeValue)]) -> Unit {
  // No-op implementation
}

pub fn NoopGauge::record(value : Double, attributes? : Array[(String, AttributeValue)]) -> Unit {
  // No-op implementation
}

pub fn NoopMeter::create_counter(name : String, unit? : String, description? : String) -> NoopCounter {
  NoopCounter::{}
}

pub fn NoopMeter::create_histogram(name : String, unit? : String, description? : String) -> NoopHistogram {
  NoopHistogram::{}
}

pub fn NoopMeter::create_up_down_counter(name : String, unit? : String, description? : String) -> NoopUpDownCounter {
  NoopUpDownCounter::{}
}

pub fn NoopMeter::create_gauge(name : String, unit? : String, description? : String) -> NoopGauge {
  NoopGauge::{}
}

pub fn NoopMeterProvider::get_meter(name : String, version? : String, schema_url? : String) -> NoopMeter {
  NoopMeter::{}
}