// 网络安全遥测测试
test "网络安全遥测测试" {
  // 测试安全相关的Span属性
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "security-tracer")
  
  let span = azimuth::Tracer::start_span(tracer, "secure-operation")
  
  // 添加安全相关属性
  azimuth::Span::add_event(span, "security.auth", Some([
    ("user.id", azimuth::StringValue("user-123")),
    ("auth.method", azimuth::StringValue("oauth2")),
    ("auth.success", azimuth::BoolValue(true)),
    ("security.level", azimuth::StringValue("high"))
  ]))
  
  // 测试安全相关的度量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "security-meter")
  
  let auth_counter = azimuth::Meter::create_counter(meter, "security.auth.attempts")
  let auth_success_counter = azimuth::Meter::create_counter(meter, "security.auth.success")
  let auth_failure_counter = azimuth::Meter::create_counter(meter, "security.auth.failures")
  let security_events_counter = azimuth::Meter::create_counter(meter, "security.events")
  
  // 记录安全度量
  azimuth::Counter::add(auth_counter, 100.0)
  azimuth::Counter::add(auth_success_counter, 85.0)
  azimuth::Counter::add(auth_failure_counter, 15.0)
  azimuth::Counter::add(security_events_counter, 25.0)
  
  azimuth::Span::end(span)
}