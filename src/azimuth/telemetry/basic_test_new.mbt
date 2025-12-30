// 基本测试用例
// 测试遥测系统的基本功能

test "metrics_basic_test" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("test_meter")
  let counter = meter.create_counter("test_counter")
  counter.add(1L)
  @test.succeed()
}

test "logs_basic_test" {
  let provider = NoopLoggerProvider::{}
  let logger = provider.get_logger("test_logger")
  logger.info("Test message")
  @test.succeed()
}

test "trace_basic_test" {
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("test_tracer")
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let (_, span) = tracer.start_span(ctx, "test_span")
  assert_eq(span.name, "test_span")
}

test "attribute_basic_test" {
  let string_attr = @azimuth.telemetry.api.common.AttributeValue::string("test")
  match string_attr {
    @azimuth.telemetry.api.common.StringValue(value) => assert_eq(value, "test")
    _ => @test.fail("Expected string value")
  }
}