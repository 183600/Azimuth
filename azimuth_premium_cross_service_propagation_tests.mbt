// Azimuth Premium Cross-Service Propagation Tests
// 高质量跨服务传播测试用例 - 专注于遥测数据在不同服务间的准确传播和转换

test "HTTP头中的遥测上下文传播" {
  let http_propagator = HTTPContextPropagator::new()
  let context_extractor = HTTPContextExtractor::new()
  
  // 创建原始遥测上下文
  let original_context = TelemetryContext::new()
  TelemetryContext::set_trace_id(original_context, "0af7651916cd43dd8448eb211c80319c")
  TelemetryContext::set_span_id(original_context, "b7ad6b7169203331")
  TelemetryContext::set_baggage_item(original_context, "user.id", "user123")
  TelemetryContext::set_baggage_item(original_context, "request.id", "req-456")
  TelemetryContext::set_sampling_decision(original_context, true)
  
  // 测试HTTP头注入
  let http_headers = HTTPHeaders::new()
  let injection_result = HTTPContextPropagator::inject(http_propagator, original_context, http_headers)
  
  match injection_result {
    Success(injected_headers) => {
      // 验证必需的追踪头
      assert_true(HTTPHeaders::contains(injected_headers, "traceparent"))
      assert_true(HTTPHeaders::contains(injected_headers, "tracestate"))
      assert_true(HTTPHeaders::contains(injected_headers, "baggage"))
      
      // 验证traceparent格式
      let traceparent = HTTPHeaders::get(injected_headers, "traceparent")
      match traceparent {
        Some(value) => {
          assert_true(value.starts_with("00-")) // 版本和格式
          assert_true(value.contains("0af7651916cd43dd8448eb211c80319c")) // trace_id
          assert_true(value.contains("b7ad6b7169203331")) // span_id
          assert_true(value.ends_with("-01")) // 采样标志
        }
        None => assert_true(false)
      }
      
      // 验证baggage头
      let baggage = HTTPHeaders::get(injected_headers, "baggage")
      match baggage {
        Some(value) => {
          assert_true(value.contains("user.id=user123"))
          assert_true(value.contains("request.id=req-456"))
        }
        None => assert_true(false)
      }
      
      // 测试HTTP头提取
      let extraction_result = HTTPContextExtractor::extract(context_extractor, injected_headers)
      match extraction_result {
        Success(extracted_context) => {
          // 验证提取的上下文
          assert_eq(TelemetryContext::trace_id(extracted_context), "0af7651916cd43dd8448eb211c80319c")
          assert_eq(TelemetryContext::span_id(extracted_context), "b7ad6b7169203331")
          assert_eq(TelemetryContext::get_baggage_item(extracted_context, "user.id"), "user123")
          assert_eq(TelemetryContext::get_baggage_item(extracted_context, "request.id"), "req-456")
          assert_true(TelemetryContext::is_sampled(extracted_context))
        }
        Error(error) => assert_true(false, "上下文提取失败: " + error.message)
      }
    }
    Error(error) => assert_true(false, "上下文注入失败: " + error.message)
  }
  
  // 测试部分头缺失的情况
  let partial_headers = HTTPHeaders::new()
  HTTPHeaders::set(partial_headers, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  // 故意缺少tracestate和baggage
  
  let partial_extraction_result = HTTPContextExtractor::extract(context_extractor, partial_headers)
  match partial_extraction_result {
    Success(partial_context) => {
      // 应该能提取基本追踪信息
      assert_eq(TelemetryContext::trace_id(partial_context), "0af7651916cd43dd8448eb211c80319c")
      assert_eq(TelemetryContext::span_id(partial_context), "b7ad6b7169203331")
      assert_true(TelemetryContext::is_sampled(partial_context))
    }
    Error(_) => assert_true(false)
  }
  
  // 测试损坏的头部处理
  let corrupted_headers = HTTPHeaders::new()
  HTTPHeaders::set(corrupted_headers, "traceparent", "invalid-format")
  HTTPHeaders::set(corrupted_headers, "baggage", "invalid-baggage-format")
  
  let corrupted_extraction_result = HTTPContextExtractor::extract(context_extractor, corrupted_headers)
  match corrupted_extraction_result {
    Success(_) => assert_true(false, "应该检测到头部格式错误"),
    Error(error) => assert_eq(error.error_type, "header_format_error")
  }
}

test "消息队列中的遥测数据传播" {
  let message_propagator = MessageQueuePropagator::new()
  let message_broker = MockMessageBroker::new()
  
  // 创建带有遥测上下文的消息
  let telemetry_context = TelemetryContext::new()
  TelemetryContext::set_trace_id(telemetry_context, "msg-trace-12345")
  TelemetryContext::set_span_id(telemetry_context, "msg-span-67890")
  TelemetryContext::set_baggage_item(telemetry_context, "correlation.id", "corr-abc")
  TelemetryContext::set_baggage_item(telemetry_context, "tenant.id", "tenant-xyz")
  
  // 创建业务消息
  let business_message = BusinessMessage::new()
  BusinessMessage::set_type(business_message, "order_processed")
  BusinessMessage::set_payload(business_message, "{\"order_id\":\"12345\",\"amount\":99.99}")
  
  // 注入遥测上下文到消息
  let enriched_message = MessageQueuePropagator::enrich_message(message_propagator, business_message, telemetry_context)
  
  // 验证消息中的遥测信息
  assert_true(BusinessMessage::has_header(enriched_message, "telemetry.trace_id"))
  assert_true(BusinessMessage::has_header(enriched_message, "telemetry.span_id"))
  assert_true(BusinessMessage::has_header(enriched_message, "telemetry.baggage"))
  
  assert_eq(BusinessMessage::get_header(enriched_message, "telemetry.trace_id"), "msg-trace-12345")
  assert_eq(BusinessMessage::get_header(enriched_message, "telemetry.span_id"), "msg-span-67890")
  
  // 测试消息发布和消费
  let topic = "telemetry.events"
  let publish_result = MockMessageBroker::publish(message_broker, topic, enriched_message)
  match publish_result {
    Success(message_id) => assert_true(message_id.length() > 0),
    Error(error) => assert_true(false, "消息发布失败: " + error.message)
  }
  
  // 模拟消费者接收消息
  let consumed_message = MockMessageBroker::consume(message_broker, topic)
  match consumed_message {
    Success(message) => {
      // 从消息中提取遥测上下文
      let extracted_context = MessageQueuePropagator::extract_context(message_propagator, message)
      match extracted_context {
        Success(context) => {
          assert_eq(TelemetryContext::trace_id(context), "msg-trace-12345")
          assert_eq(TelemetryContext::span_id(context), "msg-span-67890")
          assert_eq(TelemetryContext::get_baggage_item(context, "correlation.id"), "corr-abc")
          assert_eq(TelemetryContext::get_baggage_item(context, "tenant.id"), "tenant-xyz")
        }
        Error(error) => assert_true(false, "上下文提取失败: " + error.message)
      }
    }
    Error(error) => assert_true(false, "消息消费失败: " + error.message)
  }
  
  // 测试批量消息传播
  let batch_messages = []
  for i in 0..10 {
    let batch_context = TelemetryContext::new()
    TelemetryContext::set_trace_id(batch_context, "batch-trace-" + i.to_string())
    TelemetryContext::set_span_id(batch_context, "batch-span-" + i.to_string())
    
    let batch_message = BusinessMessage::new()
    BusinessMessage::set_type(batch_message, "batch_event")
    BusinessMessage::set_payload(batch_message, "{\"batch_id\":" + i.to_string() + "}")
    
    let enriched_batch_message = MessageQueuePropagator::enrich_message(message_propagator, batch_message, batch_context)
    batch_messages.push(enriched_batch_message)
  }
  
  let batch_publish_result = MockMessageBroker::publish_batch(message_broker, topic, batch_messages)
  match batch_publish_result {
    Success(batch_id) => assert_true(batch_id.length() > 0),
    Error(error) => assert_true(false, "批量消息发布失败: " + error.message)
  }
  
  // 验证批量消息的上下文保持
  let consumed_batch = MockMessageBroker::consume_batch(message_broker, topic, 10)
  match consumed_batch {
    Success(messages) => {
      assert_eq(messages.length(), 10)
      
      for i in 0..messages.length() {
        let extracted_batch_context = MessageQueuePropagator::extract_context(message_propagator, messages[i])
        match extracted_batch_context {
          Success(context) => {
            assert_eq(TelemetryContext::trace_id(context), "batch-trace-" + i.to_string())
            assert_eq(TelemetryContext::span_id(context), "batch-span-" + i.to_string())
          }
          Error(_) => assert_true(false)
        }
      }
    }
    Error(_) => assert_true(false)
  }
}

test "gRPC服务间的遥测传播" {
  let grpc_propagator = GRPCContextPropagator::new()
  let grpc_interceptor = GRPCTelemetryInterceptor::new(grpc_propagator)
  
  // 创建gRPC调用上下文
  let grpc_context = GRPCContext::new()
  let telemetry_context = TelemetryContext::new()
  TelemetryContext::set_trace_id(telemetry_context, "grpc-trace-123")
  TelemetryContext::set_span_id(telemetry_context, "grpc-span-456")
  TelemetryContext::set_baggage_item(telemetry_context, "user.id", "grpc-user")
  TelemetryContext::set_baggage_item(telemetry_context, "service.version", "1.2.3")
  
  // 注入遥测上下文到gRPC元数据
  let injection_result = GRPCContextPropagator::inject(grpc_propagator, telemetry_context, grpc_context)
  match injection_result {
    Success(enriched_grpc_context) => {
      // 验证gRPC元数据中的遥测信息
      assert_true(GRPCContext::has_metadata(enriched_grpc_context, "grpc-trace-bin"))
      assert_true(GRPCContext::has_metadata(enriched_grpc_context, "grpc-trace-text"))
      assert_true(GRPCContext::has_metadata(enriched_grpc_context, "grpc-baggage"))
      
      // 模拟gRPC服务端接收调用
      let server_context = GRPCContext::new()
      GRPCContext::copy_metadata(server_context, enriched_grpc_context)
      
      // 服务端提取遥测上下文
      let server_extraction_result = GRPCContextPropagator::extract(grpc_propagator, server_context)
      match server_extraction_result {
        Success(server_context) => {
          assert_eq(TelemetryContext::trace_id(server_context), "grpc-trace-123")
          assert_eq(TelemetryContext::span_id(server_context), "grpc-span-456")
          assert_eq(TelemetryContext::get_baggage_item(server_context, "user.id"), "grpc-user")
          assert_eq(TelemetryContext::get_baggage_item(server_context, "service.version"), "1.2.3")
        }
        Error(error) => assert_true(false, "服务端上下文提取失败: " + error.message)
      }
    }
    Error(error) => assert_true(false, "gRPC上下文注入失败: " + error.message)
  }
  
  // 测试gRPC拦截器
  let service_method = "telemetry.ProcessData"
  let request = GRPCRequest::new("{\"data\":\"test\"}")
  
  // 客户端拦截器
  let client_intercept_result = GRPCTelemetryInterceptor::intercept_client(grpc_interceptor, request, service_method, telemetry_context)
  match client_intercept_result {
    Success(intercepted_request) => {
      // 验证请求被正确拦截和增强
      assert_true(GRPCRequest::has_metadata(intercepted_request, "grpc-trace-bin"))
      
      // 模拟服务端拦截器处理
      let server_intercept_result = GRPCTelemetryInterceptor::intercept_server(grpc_interceptor, intercepted_request, service_method)
      match server_intercept_result {
        Success(server_telemetry_context) => {
          assert_eq(TelemetryContext::trace_id(server_telemetry_context), "grpc-trace-123")
          
          // 创建服务端响应
          let response = GRPCResponse::new "{\"result\":\"success\"}"
          let response_context = TelemetryContext::new()
          TelemetryContext::set_trace_id(response_context, "grpc-trace-123")
          TelemetryContext::set_span_id(response_context, "grpc-span-789") // 新的响应span
          
          // 服务端响应拦截
          let response_intercept_result = GRPCTelemetryInterceptor::intercept_response(grpc_interceptor, response, response_context)
          match response_intercept_result {
            Success(intercepted_response) => {
              // 验证响应包含遥测信息
              assert_true(GRPCResponse::has_trailers(intercepted_response, "grpc-trace-bin"))
            }
            Error(error) => assert_true(false, "响应拦截失败: " + error.message)
          }
        }
        Error(error) => assert_true(false, "服务端拦截失败: " + error.message)
      }
    }
    Error(error) => assert_true(false, "客户端拦截失败: " + error.message)
  }
  
  // 测试流式gRPC的遥测传播
  let stream_context = GRPCStreamContext::new()
  let stream_telemetry_context = TelemetryContext::new()
  TelemetryContext::set_trace_id(stream_telemetry_context, "stream-trace-abc")
  TelemetryContext::set_span_id(stream_telemetry_context, "stream-span-def")
  
  let stream_injection_result = GRPCContextPropagator::inject_stream(grpc_propagator, stream_telemetry_context, stream_context)
  match stream_injection_result {
    Success(enriched_stream_context) => {
      // 模拟流式消息
      let stream_messages = []
      for i in 0..5 {
        let stream_message = GRPCStreamMessage::new("{\"stream_data\":" + i.to_string() + "}")
        stream_messages.push(stream_message)
      }
      
      // 验证每个流消息都携带遥测上下文
      for message in stream_messages {
        let stream_extraction_result = GRPCContextPropagator::extract_from_stream_message(grpc_propagator, message, enriched_stream_context)
        match stream_extraction_result {
          Success(context) => {
            assert_eq(TelemetryContext::trace_id(context), "stream-trace-abc")
            assert_eq(TelemetryContext::span_id(context), "stream-span-def")
          }
          Error(_) => assert_true(false)
        }
      }
    }
    Error(error) => assert_true(false, "流式上下文注入失败: " + error.message)
  }
}

test "跨服务遥测数据的格式转换" {
  let format_converter = CrossServiceFormatConverter::new()
  let compatibility_validator = FormatCompatibilityValidator::new()
  
  // 创建标准遥测数据
  let standard_telemetry = StandardTelemetryData::new()
  StandardTelemetryData::set_trace_id(standard_telemetry, "format-test-trace")
  StandardTelemetryData::set_span_id(standard_telemetry, "format-test-span")
  StandardTelemetryData::add_tag(standard_telemetry, "service.name", "test_service")
  StandardTelemetryData::add_tag(standard_telemetry, "operation.name", "test_operation")
  StandardTelemetryData::add_metric(standard_telemetry, "duration_ms", 123.45)
  StandardTelemetryData::add_metric(standard_telemetry, "cpu_usage", 67.89)
  StandardTelemetryData::add_log(standard_telemetry, "Operation started")
  StandardTelemetryData::add_log(standard_telemetry, "Operation completed")
  
  // 测试转换为OpenTelemetry格式
  let otlp_conversion_result = CrossServiceFormatConverter::to_opentelemetry(format_converter, standard_telemetry)
  match otlp_conversion_result {
    Success(otlp_data) => {
      // 验证OTLP格式
      assert_true(OpenTelemetryData::has_trace_id(otlp_data))
      assert_true(OpenTelemetryData::has_span_id(otlp_data))
      
      assert_eq(OpenTelemetryData::trace_id(otlp_data), "format-test-trace")
      assert_eq(OpenTelemetryData::span_id(otlp_data), "format-test-span")
      
      let otlp_tags = OpenTelemetryData::get_attributes(otlp_data)
      assert_true(otlp_tags.contains_key("service.name"))
      assert_true(otlp_tags.contains_key("operation.name"))
      
      let otlp_metrics = OpenTelemetryData::get_metrics(otlp_data)
      assert_true(otlp_metrics.length() >= 2)
      
      // 验证兼容性
      let compatibility_check = FormatCompatibilityValidator::validate(compatibility_validator, standard_telemetry, otlp_data)
      assert_true(compatibility_check.is_compatible)
      assert_eq(compatibility_check.preserved_fields.length(), 6) // trace_id, span_id, 2 tags, 2 metrics
    }
    Error(error) => assert_true(false, "OTLP转换失败: " + error.message)
  }
  
  // 测试转换为Zipkin格式
  let zipkin_conversion_result = CrossServiceFormatConverter::to_zipkin(format_converter, standard_telemetry)
  match zipkin_conversion_result {
    Success(zipkin_data) => {
      // 验证Zipkin格式
      assert_eq(ZipkinData::trace_id(zipkin_data), "format-test-trace")
      assert_eq(ZipkinData::span_id(zipkin_data), "format-test-span")
      
      let zipkin_tags = ZipkinData::get_tags(zipkin_data)
      assert_true(zipkin_tags.contains_key("service.name"))
      assert_true(zipkin_tags.contains_key("operation.name"))
      
      // 验证兼容性
      let zipkin_compatibility = FormatCompatibilityValidator::validate(compatibility_validator, standard_telemetry, zipkin_data)
      assert_true(zipkin_compatibility.is_compatible)
    }
    Error(error) => assert_true(false, "Zipkin转换失败: " + error.message)
  }
  
  // 测试转换为Jaeger格式
  let jaeger_conversion_result = CrossServiceFormatConverter::to_jaeger(format_converter, standard_telemetry)
  match jaeger_conversion_result {
    Success(jaeger_data) => {
      // 验证Jaeger格式
      assert_eq(JaegerData::trace_id(jaeger_data), "format-test-trace")
      assert_eq(JaegerData::span_id(jaeger_data), "format-test-span")
      
      let jaeger_tags = JaegerData::get_tags(jaeger_data)
      assert_true(jaeger_tags.length() >= 2)
      
      let jaeger_logs = JaegerData::get_logs(jaeger_data)
      assert_true(jaeger_logs.length() >= 2)
      
      // 验证兼容性
      let jaeger_compatibility = FormatCompatibilityValidator::validate(compatibility_validator, standard_telemetry, jaeger_data)
      assert_true(jaeger_compatibility.is_compatible)
    }
    Error(error) => assert_true(false, "Jaeger转换失败: " + error.message)
  }
  
  // 测试反向转换（从其他格式转回标准格式）
  let otlp_data = otlp_conversion_result.unwrap()
  let reverse_conversion_result = CrossServiceFormatConverter::from_opentelemetry(format_converter, otlp_data)
  match reverse_conversion_result {
    Success(converted_back) => {
      // 验证反向转换的完整性
      assert_eq(StandardTelemetryData::trace_id(converted_back), StandardTelemetryData::trace_id(standard_telemetry))
      assert_eq(StandardTelemetryData::span_id(converted_back), StandardTelemetryData::span_id(standard_telemetry))
      
      let original_tags = StandardTelemetryData::get_tags(standard_telemetry)
      let converted_tags = StandardTelemetryData::get_tags(converted_back)
      assert_eq(converted_tags.length(), original_tags.length())
      
      for tag_key in original_tags.keys() {
        assert_true(converted_tags.contains_key(tag_key))
        assert_eq(converted_tags[tag_key], original_tags[tag_key])
      }
    }
    Error(error) => assert_true(false, "反向转换失败: " + error.message)
  }
  
  // 测试格式转换的性能
  let conversion_profiler = FormatConversionProfiler::new()
  let large_dataset = []
  
  // 创建大数据集进行性能测试
  for i in 0..10000 {
    let data_point = StandardTelemetryData::new()
    StandardTelemetryData::set_trace_id(data_point, "perf-test-trace-" + (i % 1000).to_string())
    StandardTelemetryData::set_span_id(data_point, "perf-test-span-" + i.to_string())
    StandardTelemetryData::add_tag(data_point, "batch.id", i.to_string())
    StandardTelemetryData::add_metric(data_point, "value", i.to_float())
    large_dataset.push(data_point)
  }
  
  // 性能测试：批量转换为OTLP
  FormatConversionProfiler::start_profiling(conversion_profiler)
  let otlp_batch_result = CrossServiceFormatConverter::batch_to_opentelemetry(format_converter, large_dataset)
  let otlp_performance = FormatConversionProfiler::stop_profiling(conversion_profiler)
  
  match otlp_batch_result {
    Success(otlp_batch) => {
      assert_eq(otlp_batch.length(), large_dataset.length())
      
      // 验证性能指标
      assert_true(otlp_performance.total_time_ms > 0)
      assert_true(otlp_performance.throughput_records_per_sec > 1000) // 至少1000记录/秒
      assert_true(otlp_performance.memory_usage_mb < 100) // 内存使用应小于100MB
    }
    Error(error) => assert_true(false, "批量OTLP转换失败: " + error.message)
  }
}

test "跨服务遥测数据的一致性验证" {
  let consistency_validator = CrossServiceConsistencyValidator::new()
  let service_chain = ServiceChain::new()
  
  // 创建服务链路
  let services = ["gateway", "auth", "user-service", "order-service", "payment-service", "notification-service"]
  
  // 模拟请求在服务链中的传播
  let initial_context = TelemetryContext::new()
  TelemetryContext::set_trace_id(initial_context, "consistency-test-trace")
  TelemetryContext::set_span_id(initial_context, "consistency-test-span")
  TelemetryContext::set_baggage_item(initial_context, "request.id", "req-12345")
  TelemetryContext::set_baggage_item(initial_context, "user.id", "user-67890")
  TelemetryContext::set_baggage_item(initial_context, "tenant.id", "tenant-abc")
  
  let service_contexts = []
  let mut current_context = initial_context
  
  // 模拟每个服务处理请求并生成子span
  for service_name in services {
    let service_span_context = TelemetryContext::create_child(current_context)
    TelemetryContext::set_service_name(service_span_context, service_name)
    TelemetryContext::add_tag(service_span_context, "service.version", "1.0.0")
    TelemetryContext::add_tag(service_span_context, "environment", "production")
    
    // 添加服务特定的baggage
    TelemetryContext::set_baggage_item(service_span_context, "processing.service", service_name)
    TelemetryContext::set_baggage_item(service_span_context, "start.time", Time::current_timestamp().to_string())
    
    service_contexts.push(service_span_context)
    current_context = service_span_context
  }
  
  // 验证服务链的一致性
  let consistency_result = CrossServiceConsistencyValidator::validate_service_chain(consistency_validator, service_contexts)
  
  // 验证基本一致性
  assert_true(consistency_result.is_consistent)
  assert_eq(consistency_result.total_services, services.length())
  assert_eq(consistency_result.consistent_services, services.length())
  assert_eq(consistency_result.inconsistent_services.length(), 0)
  
  // 验证追踪ID一致性
  for service_context in service_contexts {
    assert_eq(TelemetryContext::trace_id(service_context), "consistency-test-trace")
  }
  
  // 验证baggage传播
  for service_context in service_contexts {
    assert_eq(TelemetryContext::get_baggage_item(service_context, "request.id"), "req-12345")
    assert_eq(TelemetryContext::get_baggage_item(service_context, "user.id"), "user-67890")
    assert_eq(TelemetryContext::get_baggage_item(service_context, "tenant.id"), "tenant-abc")
  }
  
  // 验证父子关系
  for i in 1..service_contexts.length() {
    let parent_context = service_contexts[i-1]
    let child_context = service_contexts[i]
    
    assert_eq(TelemetryContext::parent_span_id(child_context), TelemetryContext::span_id(parent_context))
    assert_true(TelemetryContext::start_time(child_context) >= TelemetryContext::start_time(parent_context))
  }
  
  // 测试不一致情况的处理
  let inconsistent_context = TelemetryContext::new()
  TelemetryContext::set_trace_id(inconsistent_context, "different-trace-id") // 不同的trace_id
  TelemetryContext::set_service_name(inconsistent_context, "inconsistent-service")
  
  let mixed_contexts = service_contexts.slice(0, 3) + [inconsistent_context] + service_contexts.slice(3, service_contexts.length() - 3)
  
  let inconsistency_result = CrossServiceConsistencyValidator::validate_service_chain(consistency_validator, mixed_contexts)
  
  assert_false(inconsistency_result.is_consistent)
  assert_eq(inconsistency_result.inconsistent_services.length(), 1)
  assert_true(inconsistency_result.inconsistent_services.contains("inconsistent-service"))
  assert_eq(inconsistency_result.inconsistency_types[0], "trace_id_mismatch")
  
  // 测试部分数据丢失的恢复
  let partial_context = TelemetryContext::new()
  TelemetryContext::set_trace_id(partial_context, "consistency-test-trace") // 正确的trace_id
  TelemetryContext::set_span_id(partial_context, "partial-service-span")
  TelemetryContext::set_service_name(partial_context, "partial-service")
  // 故意缺少一些baggage项
  
  let recovery_result = CrossServiceConsistencyValidator::attempt_recovery(consistency_validator, [partial_context], initial_context)
  
  match recovery_result {
    Success(recovered_context) => {
      assert_eq(TelemetryContext::trace_id(recovered_context), "consistency-test-trace")
      // 应该恢复了部分丢失的baggage
      assert_eq(TelemetryContext::get_baggage_item(recovered_context, "request.id"), "req-12345")
      assert_eq(TelemetryContext::get_baggage_item(recovered_context, "user.id"), "user-67890")
    }
    Error(error) => assert_true(false, "恢复失败: " + error.message)
  }
  
  // 测试跨服务延迟和超时的一致性
  let latency_validator = CrossServiceLatencyValidator::new()
  
  // 添加延迟信息到服务上下文
  for i in 0..service_contexts.length() {
    let context = service_contexts[i]
    let processing_time = Random::next_int(100) + 50 // 50-150ms处理时间
    TelemetryContext::set_processing_time(context, processing_time)
    
    if i > 0 {
      let network_latency = Random::next_int(20) + 10 // 10-30ms网络延迟
      TelemetryContext::set_network_latency(context, network_latency)
    }
  }
  
  let latency_analysis = CrossServiceLatencyValidator::analyze(latency_validator, service_contexts)
  
  assert_true(latency_analysis.total_latency > 0)
  assert_true(latency_analysis.average_processing_time > 0)
  assert_true(latency_analysis.average_network_latency > 0)
  
  // 验证没有异常的延迟跳变
  assert_true(latency_analysis.latency_anomalies.length() == 0)
  
  // 测试服务依赖关系的一致性
  let dependency_validator = ServiceDependencyValidator::new()
  
  // 添加服务依赖信息
  TelemetryContext::add_dependency(service_contexts[1], "gateway") // auth依赖gateway
  TelemetryContext::add_dependency(service_contexts[2], "auth") // user-service依赖auth
  TelemetryContext::add_dependency(service_contexts[3], ["user-service", "auth"]) // order-service依赖多个服务
  TelemetryContext::add_dependency(service_contexts[4], "order-service") // payment依赖order
  TelemetryContext::add_dependency(service_contexts[5], "payment") // notification依赖payment
  
  let dependency_analysis = ServiceDependencyValidator::validate_dependencies(dependency_validator, service_contexts)
  
  assert_true(dependency_analysis.is_consistent)
  assert_eq(dependency_analysis.circular_dependencies.length(), 0) // 没有循环依赖
  assert_true(dependency_analysis.missing_dependencies.length() == 0) // 没有缺失的依赖
  
  // 验证依赖图的正确性
  let dependency_graph = dependency_analysis.dependency_graph
  assert_true(dependency_graph.has_node("gateway"))
  assert_true(dependency_graph.has_edge("auth", "gateway"))
  assert_true(dependency_graph.has_edge("user-service", "auth"))
  assert_true(dependency_graph.has_edge("order-service", "user-service"))
  assert_true(dependency_graph.has_edge("payment", "order-service"))
  assert_true(dependency_graph.has_edge("notification", "payment"))
}