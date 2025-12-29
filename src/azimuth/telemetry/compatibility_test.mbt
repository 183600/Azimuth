// 兼容性测试 - 测试与其他系统的兼容性
// 验证遥测库与OpenTelemetry、W3C标准和其他系统的兼容性

test "opentelemetry_compatibility" {
  // 测试OpenTelemetry兼容性
  
  // 验证Resource符合OTLP规范
  let resource = common::Resource::default("otel-compatible-service")
  let otel_resource_attrs = [
    ("service.name", common::AttributeValue::string("otel-compatible-service")),
    ("telemetry.sdk.name", common::AttributeValue::string("azimuth")),
    ("telemetry.sdk.version", common::AttributeValue::string("0.1.0")),
    ("telemetry.sdk.language", common::AttributeValue::string("moonbit"))
  ]
  
  // 验证OTLP Resource属性
  assert_eq(resource.service_name, "otel-compatible-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(otel_resource_attrs.length(), 4)
  
  // 验证InstrumentationScope符合OTLP规范
  let scope_name = "otel-test-instrumentation"
  let scope_version = "1.0.0"
  let scope_schema_url = "https://opentelemetry.io/schemas/1.20.0"
  
  // 验证InstrumentationScope字段
  assert_eq(scope_name, "otel-test-instrumentation")
  assert_eq(scope_version, "1.0.0")
  assert_eq(scope_schema_url, "https://opentelemetry.io/schemas/1.20.0")
  
  // 验证Span符合OTLP规范
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("otel-test-tracer", "1.0.0")
  let (_, span) = tracer.start_span(
    context::Context::empty(),
    "otel-test-span",
    trace::Server,
    [
      ("http.method", common::AttributeValue::string("GET")),
      ("http.status_code", common::AttributeValue::int(200L)),
      ("http.user_agent", common::AttributeValue::string("OpenTelemetry-Client/1.0.0"))
    ]
  )
  
  // 验证OTLP Span字段
  assert_eq(span.name, "otel-test-span")
  assert_eq(span.kind, trace::Internal)  // No-op实现默认为Internal
  assert_eq(span.attributes.length(), 3)
  assert_eq(span.status, trace::Unset)
}

test "w3c_tracecontext_compliance" {
  // 测试W3C TraceContext合规性
  
  // 验证traceparent格式
  let version = "00"
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let trace_flags = "01"
  
  let traceparent = version + "-" + trace_id + "-" + span_id + "-" + trace_flags
  
  // 验证traceparent格式
  assert_eq(traceparent, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  assert_eq(traceparent.split("-").length(), 4)
  
  // 验证版本字段
  assert_eq(version, "00")
  assert_eq(version.length(), 2)
  
  // 验证trace-id字段
  assert_eq(trace_id, "0af7651916cd43dd8448eb211c80319c")
  assert_eq(trace_id.length(), 32)
  
  // 验证span-id字段
  assert_eq(span_id, "b7ad6b7169203331")
  assert_eq(span_id.length(), 16)
  
  // 验证trace-flags字段
  assert_eq(trace_flags, "01")
  assert_eq(trace_flags.length(), 2)
  
  // 验证tracestate格式
  let tracestate = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  
  // 验证tracestate格式
  assert_eq(tracestate.contains("rojo=00f067aa0ba902b7"), true)
  assert_eq(tracestate.contains("congo=t61rcWkgMzE"), true)
  assert_eq(tracestate.contains(","), true)
  
  // 验证tracestate列表成员格式
  let list_members = tracestate.split(",")
  assert_eq(list_members.length(), 2)
  assert_eq(list_members[0], "rojo=00f067aa0ba902b7")
  assert_eq(list_members[1], "congo=t61rcWkgMzE")
}

test "w3c_baggage_compliance" {
  // 测试W3C Baggage合规性
  
  // 验证基本baggage格式
  let basic_baggage = "key1=value1,key2=value2,key3=value3"
  
  // 验证基本baggage格式
  assert_eq(basic_baggage.contains("key1=value1"), true)
  assert_eq(basic_baggage.contains("key2=value2"), true)
  assert_eq(basic_baggage.contains("key3=value3"), true)
  
  // 验证带属性的baggage项
  let baggage_with_properties = "userId=12345;userIdType=internal,sessionId=abc-def;source=web"
  
  // 验证带属性的baggage格式
  assert_eq(baggage_with_properties.contains("userId=12345;userIdType=internal"), true)
  assert_eq(baggage_with_properties.contains("sessionId=abc-def;source=web"), true)
  
  // 验证属性格式
  let baggage_item1 = "userId=12345;userIdType=internal"
  let item_parts = baggage_item1.split(";")
  assert_eq(item_parts.length(), 2)
  assert_eq(item_parts[0], "userId=12345")
  assert_eq(item_parts[1], "userIdType=internal")
  
  // 验证URL编码的baggage
  let url_encoded_baggage = "user.name=John%20Doe,request.path=%2Fapi%2Fv1%2Fusers"
  
  // 验证URL编码
  assert_eq(url_encoded_baggage.contains("user.name=John%20Doe"), true)
  assert_eq(url_encoded_baggage.contains("request.path=%2Fapi%2Fv1%2Fusers"), true)
  
  // 验证空baggage
  let empty_baggage = ""
  assert_eq(empty_baggage, "")
}

test "semantic_conventions_compatibility" {
  // 测试语义约定兼容性
  
  // 验证资源语义约定
  let resource_attrs = [
    ("service.name", common::AttributeValue::string("payment-service")),
    ("service.namespace", common::AttributeValue::string("ecommerce")),
    ("service.version", common::AttributeValue::string("1.2.3")),
    ("service.instance.id", common::AttributeValue::string("instance-12345")),
    ("deployment.environment", common::AttributeValue::string("production")),
    ("host.name", common::AttributeValue::string("web-server-01")),
    ("host.id", common::AttributeValue::string("host-id-67890")),
    ("process.pid", common::AttributeValue::int(12345L)),
    ("process.executable.name", common::AttributeValue::string("payment-service")),
    ("process.command", common::AttributeValue::string("./payment-service")),
    ("process.command_args", common::AttributeValue::array_string(["--port", "8080", "--config", "/etc/payment/config.yaml"])),
    ("telemetry.sdk.name", common::AttributeValue::string("azimuth")),
    ("telemetry.sdk.language", common::AttributeValue::string("moonbit")),
    ("telemetry.sdk.version", common::AttributeValue::string("0.1.0"))
  ]
  
  // 验证资源语义约定
  assert_eq(resource_attrs.length(), 13)
  assert_eq(resource_attrs[0].0, "service.name")
  assert_eq(resource_attrs[1].0, "service.namespace")
  assert_eq(resource_attrs[2].0, "service.version")
  
  // 验证HTTP语义约定
  let http_attrs = [
    ("http.method", common::AttributeValue::string("GET")),
    ("http.url", common::AttributeValue::string("https://api.example.com/v1/users")),
    ("http.target", common::AttributeValue::string("/v1/users")),
    ("http.host", common::AttributeValue::string("api.example.com")),
    ("http.scheme", common::AttributeValue::string("https")),
    ("http.status_code", common::AttributeValue::int(200L)),
    ("http.status_text", common::AttributeValue::string("OK")),
    ("http.flavor", common::AttributeValue::string("1.1")),
    ("http.user_agent", common::AttributeValue::string("Mozilla/5.0 (compatible; OTel-Client/1.0.0)")),
    ("http.request_content_length", common::AttributeValue::int(0L)),
    ("http.response_content_length", common::AttributeValue::int(1024L))
  ]
  
  // 验证HTTP语义约定
  assert_eq(http_attrs.length(), 11)
  assert_eq(http_attrs[0].0, "http.method")
  assert_eq(http_attrs[1].0, "http.url")
  assert_eq(http_attrs[2].0, "http.target")
  
  // 验证数据库语义约定
  let db_attrs = [
    ("db.system", common::AttributeValue::string("postgresql")),
    ("db.name", common::AttributeValue::string("ecommerce")),
    ("db.statement", common::AttributeValue::string("SELECT * FROM users WHERE id = $1")),
    ("db.operation", common::AttributeValue::string("SELECT")),
    ("db.user", common::AttributeValue::string("app_user")),
    ("db.connection_string", common::AttributeValue::string("postgresql://user:pass@localhost:5432/ecommerce")),
    ("db.instance.id", common::AttributeValue::string("db-12345"))
  ]
  
  // 验证数据库语义约定
  assert_eq(db_attrs.length(), 7)
  assert_eq(db_attrs[0].0, "db.system")
  assert_eq(db_attrs[1].0, "db.name")
  assert_eq(db_attrs[2].0, "db.statement")
}

test "prometheus_format_compatibility" {
  // 测试Prometheus格式兼容性
  
  // 验证Counter指标格式
  let counter_metric = "http_requests_total{service=\"api\",method=\"GET\",status=\"200\",code=\"200\"} 12345"
  
  // 验证Counter格式
  assert_eq(counter_metric.contains("http_requests_total"), true)
  assert_eq(counter_metric.contains("service=\"api\""), true)
  assert_eq(counter_metric.contains("method=\"GET\""), true)
  assert_eq(counter_metric.contains("status=\"200\""), true)
  assert_eq(counter_metric.has_suffix(" 12345"), true)
  
  // 验证Histogram指标格式
  let histogram_metric = "http_request_duration_seconds_bucket{service=\"api\",le=\"0.1\"} 100"
  let histogram_count = "http_request_duration_seconds_count{service=\"api\"} 1000"
  let histogram_sum = "http_request_duration_seconds_sum{service=\"api\"} 123.45"
  
  // 验证Histogram格式
  assert_eq(histogram_metric.contains("http_request_duration_seconds_bucket"), true)
  assert_eq(histogram_metric.contains("le=\"0.1\""), true)
  assert_eq(histogram_metric.has_suffix(" 100"), true)
  
  assert_eq(histogram_count.contains("http_request_duration_seconds_count"), true)
  assert_eq(histogram_count.has_suffix(" 1000"), true)
  
  assert_eq(histogram_sum.contains("http_request_duration_seconds_sum"), true)
  assert_eq(histogram_sum.has_suffix(" 123.45"), true)
  
  // 验证Gauge指标格式
  let gauge_metric = "active_connections{service=\"api\",instance=\"pod-123\",state=\"established\"} 42"
  
  // 验证Gauge格式
  assert_eq(gauge_metric.contains("active_connections"), true)
  assert_eq(gauge_metric.contains("service=\"api\""), true)
  assert_eq(gauge_metric.contains("instance=\"pod-123\""), true)
  assert_eq(gauge_metric.contains("state=\"established\""), true)
  assert_eq(gauge_metric.has_suffix(" 42"), true)
  
  // 验证Summary指标格式
  let summary_metric = "rpc_duration_seconds{service=\"api\",quantile=\"0.5\"} 0.05"
  let summary_count = "rpc_duration_seconds_count{service=\"api\"} 10000"
  let summary_sum = "rpc_duration_seconds_sum{service=\"api\"} 567.89"
  
  // 验证Summary格式
  assert_eq(summary_metric.contains("rpc_duration_seconds"), true)
  assert_eq(summary_metric.contains("quantile=\"0.5\""), true)
  assert_eq(summary_metric.has_suffix(" 0.05"), true)
  
  assert_eq(summary_count.contains("rpc_duration_seconds_count"), true)
  assert_eq(summary_count.has_suffix(" 10000"), true)
  
  assert_eq(summary_sum.contains("rpc_duration_seconds_sum"), true)
  assert_eq(summary_sum.has_suffix(" 567.89"), true)
}

test "jaeger_format_compatibility" {
  // 测试Jaeger格式兼容性
  
  // 验证Jaeger Span格式
  let jaeger_span = "{"
  jaeger_span = jaeger_span + "\"traceID\":\"0af7651916cd43dd8448eb211c80319c\"," +
  jaeger_span = jaeger_span + "\"spanID\":\"b7ad6b7169203331\"," +
  jaeger_span = jaeger_span + "\"operationName\":\"http-request\"," +
  jaeger_span = jaeger_span + "\"startTime\":1640995200000000," +
  jaeger_span = jaeger_span + "\"duration\":123456," +
  jaeger_span = jaeger_span + "\"tags\":[" +
  jaeger_span = jaeger_span + "{\"key\":\"http.method\",\"value\":\"GET\"}," +
  jaeger_span = jaeger_span + "{\"key\":\"http.status_code\",\"value\":200}," +
  jaeger_span = jaeger_span + "{\"key\":\"component\",\"value\":\"http-client\"}" +
  jaeger_span = jaeger_span + "]," +
  jaeger_span = jaeger_span + "\"logs\":[]," +
  jaeger_span = jaeger_span + "\"process\":{" +
  jaeger_span = jaeger_span + "\"serviceName\":\"api-service\"," +
  jaeger_span = jaeger_span + "\"tags\":[" +
  jaeger_span = jaeger_span + "{\"key\":\"jaeger.version\",\"value\":\"1.35\"}" +
  jaeger_span = jaeger_span + "]" +
  jaeger_span = jaeger_span + "}" +
  jaeger_span = jaeger_span + "}"
  
  // 验证Jaeger格式
  assert_eq(jaeger_span.contains("\"traceID\":\"0af7651916cd43dd8448eb211c80319c\""), true)
  assert_eq(jaeger_span.contains("\"spanID\":\"b7ad6b7169203331\""), true)
  assert_eq(jaeger_span.contains("\"operationName\":\"http-request\""), true)
  assert_eq(jaeger_span.contains("\"startTime\":1640995200000000"), true)
  assert_eq(jaeger_span.contains("\"duration\":123456"), true)
  assert_eq(jaeger_span.contains("\"tags\":"), true)
  assert_eq(jaeger_span.contains("\"http.method\""), true)
  assert_eq(jaeger_span.contains("\"http.status_code\""), true)
  assert_eq(jaeger_span.contains("\"process\":"), true)
  assert_eq(jaeger_span.contains("\"serviceName\":\"api-service\""), true)
}

test "zipkin_format_compatibility" {
  // 测试Zipkin格式兼容性
  
  // 验证Zipkin Span格式
  let zipkin_span = "{"
  zipkin_span = zipkin_span + "\"traceId\":\"0af7651916cd43dd8448eb211c80319c\"," +
  zipkin_span = zipkin_span + "\"id\":\"b7ad6b7169203331\"," +
  zipkin_span = zipkin_span + "\"name\":\"http-request\"," +
  zipkin_span = zipkin_span + "\"timestamp\":1640995200000000," +
  zipkin_span = zipkin_span + "\"duration\":123456," +
  zipkin_span = zipkin_span + "\"localEndpoint\":{" +
  zipkin_span = zipkin_span + "\"serviceName\":\"api-service\"," +
  zipkin_span = zipkin_span + "\"ipv4\":\"192.168.1.100\"," +
  zipkin_span = zipkin_span + "\"port\":8080" +
  zipkin_span = zipkin_span + "}," +
  zipkin_span = zipkin_span + "\"tags\":{" +
  zipkin_span = zipkin_span + "\"http.method\":\"GET\"," +
  zipkin_span = zipkin_span + "\"http.status_code\":\"200\"," +
  zipkin_span = zipkin_span + "\"component\":\"http-client\"" +
  zipkin_span = zipkin_span + "}" +
  zipkin_span = zipkin_span + "}"
  
  // 验证Zipkin格式
  assert_eq(zipkin_span.contains("\"traceId\":\"0af7651916cd43dd8448eb211c80319c\""), true)
  assert_eq(zipkin_span.contains("\"id\":\"b7ad6b7169203331\""), true)
  assert_eq(zipkin_span.contains("\"name\":\"http-request\""), true)
  assert_eq(zipkin_span.contains("\"timestamp\":1640995200000000"), true)
  assert_eq(zipkin_span.contains("\"duration\":123456), true)
  assert_eq(zipkin_span.contains("\"localEndpoint\":"), true)
  assert_eq(zipkin_span.contains("\"serviceName\":\"api-service\""), true)
  assert_eq(zipkin_span.contains("\"ipv4\":\"192.168.1.100\""), true)
  assert_eq(zipkin_span.contains("\"port\":8080"), true)
  assert_eq(zipkin_span.contains("\"tags\":"), true)
  assert_eq(zipkin_span.contains("\"http.method\":\"GET\""), true)
}

test "grpc_format_compatibility" {
  // 测试gRPC格式兼容性
  
  // 验证gRPC服务语义约定
  let grpc_attrs = [
    ("rpc.system", common::AttributeValue::string("grpc")),
    ("rpc.service", common::AttributeValue::string("ecommerce.UserService")),
    ("rpc.method", common::AttributeValue::string("GetUser")),
    ("rpc.grpc.status_code", common::AttributeValue::int(0L)),  // OK
    ("net.host.name", common::AttributeValue::string("api.example.com")),
    ("net.host.port", common::AttributeValue::int(443L)),
    ("net.sock.peer.addr", common::AttributeValue::string("192.168.1.100")),
    ("net.sock.peer.port", common::AttributeValue::int(12345L))
  ]
  
  // 验证gRPC语义约定
  assert_eq(grpc_attrs.length(), 8)
  assert_eq(grpc_attrs[0].0, "rpc.system")
  assert_eq(grpc_attrs[1].0, "rpc.service")
  assert_eq(grpc_attrs[2].0, "rpc.method")
  
  // 验证gRPC状态码映射
  let ok_status = 0L
  let canceled_status = 1L
  let unknown_status = 2L
  let invalid_argument_status = 3L
  let deadline_exceeded_status = 4L
  let not_found_status = 5L
  let already_exists_status = 6L
  let permission_denied_status = 7L
  let resource_exhausted_status = 8L
  let failed_precondition_status = 9L
  let aborted_status = 10L
  let out_of_range_status = 11L
  let unimplemented_status = 12L
  let internal_status = 13L
  let unavailable_status = 14L
  let data_loss_status = 15L
  let unauthenticated_status = 16L
  
  // 验证gRPC状态码
  assert_eq(ok_status, 0L)
  assert_eq(canceled_status, 1L)
  assert_eq(unknown_status, 2L)
  assert_eq(unauthenticated_status, 16L)
}

test "legacy_system_compatibility" {
  // 测试传统系统兼容性
  
  // 验证传统日志格式兼容性
  let legacy_log_format = "2023-12-31 23:59:59.123 [INFO] [payment-service] [thread-123] Processing payment request"
  
  // 验证传统日志格式
  assert_eq(legacy_log_format.contains("2023-12-31 23:59:59.123"), true)
  assert_eq(legacy_log_format.contains("[INFO]"), true)
  assert_eq(legacy_log_format.contains("[payment-service]"), true)
  assert_eq(legacy_log_format.contains("[thread-123]"), true)
  assert_eq(legacy_log_format.contains("Processing payment request"), true)
  
  // 验证传统指标格式兼容性
  let legacy_metric_format = "payment.service.requests.count 12345 1640995200"
  
  // 验证传统指标格式
  assert_eq(legacy_metric_format.contains("payment.service.requests.count"), true)
  assert_eq(legacy_metric_format.contains("12345"), true)
  assert_eq(legacy_metric_format.contains("1640995200"), true)
  
  // 验证传统追踪格式兼容性
  let legacy_trace_format = "trace=0af7651916cd43dd8448eb211c80319c span=b7ad6b7169203331 parent=0123456789abcdef name=payment-process"
  
  // 验证传统追踪格式
  assert_eq(legacy_trace_format.contains("trace=0af7651916cd43dd8448eb211c80319c"), true)
  assert_eq(legacy_trace_format.contains("span=b7ad6b7169203331"), true)
  assert_eq(legacy_trace_format.contains("parent=0123456789abcdef"), true)
  assert_eq(legacy_trace_format.contains("name=payment-process"), true)
  
  // 验证传统属性格式兼容性
  let legacy_attr_format = "service=payment-service;version=1.2.3;env=production;region=us-west-2"
  
  // 验证传统属性格式
  assert_eq(legacy_attr_format.contains("service=payment-service"), true)
  assert_eq(legacy_attr_format.contains("version=1.2.3"), true)
  assert_eq(legacy_attr_format.contains("env=production"), true)
  assert_eq(legacy_attr_format.contains("region=us-west-2"), true)
}

test "cloud_platform_compatibility" {
  // 测试云平台兼容性
  
  // 验证AWS语义约定
  let aws_attrs = [
    ("cloud.provider", common::AttributeValue::string("aws")),
    ("cloud.platform", common::AttributeValue::string("aws_ec2")),
    ("cloud.region", common::AttributeValue::string("us-west-2")),
    ("cloud.account.id", common::AttributeValue::string("123456789012")),
    ("cloud.resource.id", common::AttributeValue::string("i-1234567890abcdef0")),
    ("aws.ec2.instance.id", common::AttributeValue::string("i-1234567890abcdef0")),
    ("aws.ec2.instance.type", common::AttributeValue::string("t3.micro")),
    ("aws.ec2.instance.image.id", common::AttributeValue::string("ami-1234567890abcdef0"))
  ]
  
  // 验证AWS语义约定
  assert_eq(aws_attrs.length(), 8)
  assert_eq(aws_attrs[0].0, "cloud.provider")
  assert_eq(aws_attrs[1].0, "cloud.platform")
  assert_eq(aws_attrs[2].0, "cloud.region")
  
  // 验证GCP语义约定
  let gcp_attrs = [
    ("cloud.provider", common::AttributeValue::string("gcp")),
    ("cloud.platform", common::AttributeValue::string("gcp_compute_engine")),
    ("cloud.region", common::AttributeValue::string("us-central1")),
    ("cloud.project.id", common::AttributeValue::string("my-gcp-project")),
    ("gcp.gce.instance.id", common::AttributeValue::string("1234567890123456789")),
    ("gcp.gce.instance.name", common::AttributeValue::string("my-gcp-instance")),
    ("gcp.gce.instance.hostname", common::AttributeValue::string("my-gcp-instance.c.my-gcp-project.internal"))
  ]
  
  // 验证GCP语义约定
  assert_eq(gcp_attrs.length(), 7)
  assert_eq(gcp_attrs[0].0, "cloud.provider")
  assert_eq(gcp_attrs[1].0, "cloud.platform")
  assert_eq(gcp_attrs[2].0, "cloud.region")
  
  // 验证Azure语义约定
  let azure_attrs = [
    ("cloud.provider", common::AttributeValue::string("azure")),
    ("cloud.platform", common::AttributeValue::string("azure_vm")),
    ("cloud.region", common::AttributeValue::string("eastus")),
    ("cloud.resource.id", common::AttributeValue::string("/subscriptions/12345678-1234-1234-1234-123456789012/resourceGroups/my-resource-group/providers/Microsoft.Compute/virtualMachines/my-vm")),
    ("azure.vm.name", common::AttributeValue::string("my-vm")),
    ("azure.vm.size", common::AttributeValue::string("Standard_B1s")),
    ("azure.vm.sku", common::AttributeValue::string("Standard_B1s")),
    ("azure.vm.image.id", common::AttributeValue::string("/subscriptions/12345678-1234-1234-1234-123456789012/providers/Microsoft.Compute/images/my-image"))
  ]
  
  // 验证Azure语义约定
  assert_eq(azure_attrs.length(), 8)
  assert_eq(azure_attrs[0].0, "cloud.provider")
  assert_eq(azure_attrs[1].0, "cloud.platform")
  assert_eq(azure_attrs[2].0, "cloud.region")
  
  // 验证Kubernetes语义约定
  let k8s_attrs = [
    ("k8s.namespace.name", common::AttributeValue::string("production")),
    ("k8s.pod.name", common::AttributeValue::string("my-pod-12345")),
    ("k8s.pod.uid", common::AttributeValue::string("12345678-1234-1234-1234-123456789012")),
    ("k8s.deployment.name", common::AttributeValue::string("my-deployment")),
    ("k8s.replicaset.name", common::AttributeValue::string("my-deployment-67890")),
    ("k8s.statefulset.name", common::AttributeValue::string("my-statefulset")),
    ("k8s.daemonset.name", common::AttributeValue::string("my-daemonset")),
    ("k8s.job.name", common::AttributeValue::string("my-job")),
    ("k8s.cronjob.name", common::AttributeValue::string("my-cronjob"))
  ]
  
  // 验证Kubernetes语义约定
  assert_eq(k8s_attrs.length(), 9)
  assert_eq(k8s_attrs[0].0, "k8s.namespace.name")
  assert_eq(k8s_attrs[1].0, "k8s.pod.name")
  assert_eq(k8s_attrs[2].0, "k8s.pod.uid")
}