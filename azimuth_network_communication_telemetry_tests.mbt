// 网络通信和遥测测试
// 测试Azimuth遥测系统的网络通信和遥测功能

test "HTTP客户端遥测集成" {
  // 测试HTTP客户端遥测集成功能
  let telemetry_http_client = TelemetryHttpClient::new()
  
  // 配置遥测
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "http.client")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "http.client")
  
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let response_time_histogram = Meter::create_histogram(meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  
  TelemetryHttpClient::configure_telemetry(telemetry_http_client, tracer, request_counter, response_time_histogram)
  
  // 创建HTTP请求
  let request = HttpRequest::new("GET", "https://api.example.com/users")
  HttpRequest::add_header(request, "Accept", "application/json")
  HttpRequest::add_header(request, "User-Agent", "Azimuth-Telemetry/1.0")
  
  // 发送请求并记录遥测数据
  let response = TelemetryHttpClient::send(telemetry_http_client, request)
  
  // 验证响应
  assert_true(HttpResponse::is_success(response))
  assert_eq(HttpResponse::get_status_code(response), 200)
  
  // 验证遥测数据已记录
  let spans = Tracer::get_spans(tracer)
  assert_eq(Array::length(spans), 1)
  
  let span = Array::get(spans, 0)
  assert_eq(Span::get_name(span), "HTTP GET https://api.example.com/users")
  assert_true(Span::has_attribute(span, "http.method"))
  assert_true(Span::has_attribute(span, "http.url"))
  assert_true(Span::has_attribute(span, "http.status_code"))
  
  // 验证度量数据
  let counter_value = Counter::get_value(request_counter)
  assert_eq(counter_value, 1.0)
  
  let histogram_data = Histogram::get_data(response_time_histogram)
  assert_true(HistogramData::get_count(histogram_data) > 0)
  
  // 测试错误请求
  let error_request = HttpRequest::new("GET", "https://api.example.com/nonexistent")
  let error_response = TelemetryHttpClient::send(telemetry_http_client, error_request)
  
  // 验证错误响应
  assert_eq(HttpResponse::get_status_code(error_response), 404)
  
  // 验证错误遥测数据
  let error_spans = Tracer::get_spans(tracer)
  assert_eq(Array::length(error_spans), 2)
  
  let error_span = Array::get(error_spans, 1)
  assert_eq(Span::get_status(error_span), Error)
  assert_true(Span::has_attribute(error_span, "http.status_code"))
  assert_eq(Span::get_attribute(error_span, "http.status_code"), "404")
}

test "网络延迟和带宽监控" {
  // 测试网络延迟和带宽监控功能
  let network_monitor = NetworkMonitor::new()
  
  // 配置监控目标
  let targets = [
    NetworkTarget::new("api.example.com", 443, HTTPS),
    NetworkTarget::new("db.example.com", 5432, TCP),
    NetworkTarget::new("cache.example.com", 6379, TCP)
  ]
  
  for target in targets {
    NetworkMonitor::add_target(network_monitor, target)
  }
  
  // 开始监控
  NetworkMonitor::start_monitoring(network_monitor)
  
  // 执行网络测试
  for i in 0..10 {
    let latency_results = NetworkMonitor::measure_latency(network_monitor)
    let bandwidth_results = NetworkMonitor::measure_bandwidth(network_monitor)
    
    // 验证延迟结果
    for result in latency_results {
      assert_true(LatencyResult::has_target(result))
      assert_true(LatencyResult::get_latency_ms(result) >= 0.0)
    }
    
    // 验证带宽结果
    for result in bandwidth_results {
      assert_true(BandwidthResult::has_target(result))
      assert_true(BandwidthResult::get_throughput_mbps(result) >= 0.0)
    }
    
    // 等待下一次测量
    NetworkMonitor::wait(network_monitor, 1000) // 等待1秒
  }
  
  // 停止监控
  NetworkMonitor::stop_monitoring(network_monitor)
  
  // 获取监控报告
  let latency_report = NetworkMonitor::get_latency_report(network_monitor)
  let bandwidth_report = NetworkMonitor::get_bandwidth_report(network_monitor)
  
  // 验证延迟报告
  assert_true(LatencyReport::has_target_statistics(latency_report))
  
  for target in targets {
    let stats = LatencyReport::get_statistics(latency_report, target)
    assert_true(NetworkStats::has_min(stats))
    assert_true(NetworkStats::has_max(stats))
    assert_true(NetworkStats::has_avg(stats))
    assert_true(NetworkStats::has_p95(stats))
    assert_true(NetworkStats::has_p99(stats))
    
    // 验证统计值的合理性
    let min_latency = NetworkStats::get_min(stats)
    let max_latency = NetworkStats::get_max(stats)
    let avg_latency = NetworkStats::get_avg(stats)
    let p95_latency = NetworkStats::get_p95(stats)
    
    assert_true(min_latency <= avg_latency)
    assert_true(avg_latency <= max_latency)
    assert_true(avg_latency <= p95_latency)
    assert_true(p95_latency <= max_latency)
  }
  
  // 验证带宽报告
  assert_true(BandwidthReport::has_target_statistics(bandwidth_report))
  
  for target in targets {
    let stats = BandwidthReport::get_statistics(bandwidth_report, target)
    assert_true(NetworkStats::has_min(stats))
    assert_true(NetworkStats::has_max(stats))
    assert_true(NetworkStats::has_avg(stats))
    
    // 验证带宽统计值的合理性
    let min_bandwidth = NetworkStats::get_min(stats)
    let max_bandwidth = NetworkStats::get_max(stats)
    let avg_bandwidth = NetworkStats::get_avg(stats)
    
    assert_true(min_bandwidth <= avg_bandwidth)
    assert_true(avg_bandwidth <= max_bandwidth)
  }
}

test "连接池遥测和监控" {
  // 测试连接池遥测和监控功能
  let connection_pool = ConnectionPool::new("db.example.com", 5432, 10)
  
  // 配置连接池遥测
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "connection.pool")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "connection.pool")
  
  let active_connections_gauge = Meter::create_gauge(meter, "connection.pool.active", Some("Active connections"), Some("count"))
  let idle_connections_gauge = Meter::create_gauge(meter, "connection.pool.idle", Some("Idle connections"), Some("count"))
  let connection_creation_counter = Meter::create_counter(meter, "connection.pool.created.total", Some("Total connections created"), Some("count"))
  let connection_lifetime_histogram = Meter::create_histogram(meter, "connection.lifetime", Some("Connection lifetime"), Some("ms"))
  
  ConnectionPool::configure_telemetry(connection_pool, tracer, active_connections_gauge, idle_connections_gauge, connection_creation_counter, connection_lifetime_histogram)
  
  // 获取连接
  let connections = []
  for i in 0..5 {
    let connection = ConnectionPool::get_connection(connection_pool)
    Array::push(connections, connection)
    
    // 验证活跃连接数
    let active_count = Gauge::get_value(active_connections_gauge)
    assert_eq(active_count, (i + 1).to_float())
    
    // 验证空闲连接数
    let idle_count = Gauge::get_value(idle_connections_gauge)
    assert_eq(idle_count, (10 - (i + 1)).to_float())
  }
  
  // 使用连接
  for i in 0..5 {
    let connection = Array::get(connections, i)
    Connection::execute_query(connection, "SELECT * FROM users WHERE id = " + (i + 1).to_string())
  }
  
  // 释放连接
  for i in 0..5 {
    let connection = Array::get(connections, i)
    ConnectionPool::release_connection(connection_pool, connection)
    
    // 验证活跃连接数
    let active_count = Gauge::get_value(active_connections_gauge)
    assert_eq(active_count, (5 - (i + 1)).to_float())
    
    // 验证空闲连接数
    let idle_count = Gauge::get_value(idle_connections_gauge)
    assert_eq(idle_count, (5 + (i + 1)).to_float())
  }
  
  // 验证连接创建计数
  let created_count = Counter::get_value(connection_creation_counter)
  assert_eq(created_count, 5.0)
  
  // 验证连接生命周期
  let lifetime_data = Histogram::get_data(connection_lifetime_histogram)
  assert_true(HistogramData::get_count(lifetime_data) >= 0)
  
  // 获取连接池统计
  let pool_stats = ConnectionPool::get_statistics(connection_pool)
  
  // 验证统计信息
  assert_eq(PoolStats::get_total_connections(pool_stats), 5)
  assert_eq(PoolStats::get_active_connections(pool_stats), 0)
  assert_eq(PoolStats::get_idle_connections(pool_stats), 5)
  assert_eq(PoolStats::get_max_connections(pool_stats), 10)
  assert_true(PoolStats::get_creation_time(pool_stats) > 0)
  assert_true(PoolStats::get_last_access_time(pool_stats) > 0)
  
  // 测试连接池满的情况
  let overflow_connections = []
  for i in 0..15 { // 尝试获取15个连接，但池中只有10个
    let connection = ConnectionPool::get_connection_with_timeout(connection_pool, 100) // 100ms超时
    if Option::is_some(connection) {
      Array::push(overflow_connections, Option::unwrap(connection))
    }
  }
  
  // 验证最多只能获取10个连接
  assert_eq(Array::length(overflow_connections), 10)
  
  // 释放所有连接
  for connection in overflow_connections {
    ConnectionPool::release_connection(connection_pool, connection)
  }
}

test "网络拓扑发现和映射" {
  // 测试网络拓扑发现和映射功能
  let topology_discoverer = NetworkTopologyDiscoverer::new()
  
  // 配置发现范围
  let discovery_config = DiscoveryConfig::new()
  DiscoveryConfig::set_scan_range(discovery_config, "192.168.1.0/24")
  DiscoveryConfig::set_ports(discovery_config, [22, 80, 443, 3306, 5432, 6379, 9200])
  DiscoveryConfig::set_timeout(discovery_config, 5000) // 5秒超时
  DiscoveryConfig::set_max_concurrent_scans(discovery_config, 50)
  
  NetworkTopologyDiscoverer::configure(topology_discoverer, discovery_config)
  
  // 开始发现
  NetworkTopologyDiscoverer::start_discovery(topology_discoverer)
  
  // 等待发现完成
  NetworkTopologyDiscoverer::wait_for_completion(topology_discoverer)
  
  // 获取拓扑结果
  let topology = NetworkTopologyDiscoverer::get_topology(topology_discoverer)
  
  // 验证拓扑结构
  assert_true(Topology::has_nodes(topology))
  assert_true(Topology::has_edges(topology))
  
  let nodes = Topology::get_nodes(topology)
  let edges = Topology::get_edges(topology)
  
  assert_true(Array::length(nodes) >= 0)
  assert_true(Array::length(edges) >= 0)
  
  // 验证节点信息
  for node in nodes {
    assert_true(Node::has_id(node))
    assert_true(Node::has_ip_address(node))
    assert_true(Node::has_hostname(node))
    assert_true(Node::has_services(node))
    
    let services = Node::get_services(node)
    for service in services {
      assert_true(Service::has_port(service))
      assert_true(Service::has_protocol(service))
      assert_true(Service::has_state(service))
    }
  }
  
  // 验证边信息
  for edge in edges {
    assert_true(Edge::has_source(edge))
    assert_true(Edge::has_target(edge))
    assert_true(Edge::has_latency(edge))
    assert_true(Edge::has_bandwidth(edge))
    
    // 验证延迟和带宽的合理性
    let latency = Edge::get_latency(edge)
    let bandwidth = Edge::get_bandwidth(edge)
    
    assert_true(latency >= 0.0)
    assert_true(bandwidth >= 0.0)
  }
  
  // 创建拓扑映射
  let topology_mapper = TopologyMapper::new()
  let topology_map = TopologyMapper::create_map(topology_mapper, topology)
  
  // 验证拓扑映射
  assert_true(TopologyMap::has_layers(topology_map))
  
  let layers = TopologyMap::get_layers(topology_map)
  assert_true(Array::length(layers) >= 1)
  
  // 验证层结构
  for layer in layers {
    assert_true(Layer::has_name(layer))
    assert_true(Layer::has_nodes(layer))
    assert_true(Layer::has_edges(layer))
  }
  
  // 导出拓扑数据
  let topology_exporter = TopologyExporter::new()
  let json_topology = TopologyExporter::export_to_json(topology_exporter, topology)
  
  // 验证导出数据
  assert_true(String::length(json_topology) > 0)
  assert_true(JsonValidator::is_valid(json_topology))
  
  // 导出拓扑映射
  let svg_map = TopologyExporter::export_map_to_svg(topology_exporter, topology_map)
  
  // 验证SVG导出
  assert_true(String::length(svg_map) > 0)
  assert_true(String::contains(svg_map, "<svg"))
  assert_true(String::contains(svg_map, "</svg>"))
}

test "网络流量分析和捕获" {
  // 测试网络流量分析和捕获功能
  let traffic_analyzer = NetworkTrafficAnalyzer::new()
  
  // 配置流量捕获
  let capture_config = CaptureConfig::new()
  CaptureConfig::set_interface(capture_config, "eth0")
  CaptureConfig::set_filter(capture_config, "tcp port 80 or tcp port 443")
  CaptureConfig::set_buffer_size(capture_config, 1024 * 1024) // 1MB缓冲区
  CaptureConfig::set_timeout(capture_config, 10000) // 10秒超时
  
  NetworkTrafficAnalyzer::configure_capture(traffic_analyzer, capture_config)
  
  // 开始捕获
  NetworkTrafficAnalyzer::start_capture(traffic_analyzer)
  
  // 模拟网络流量
  let traffic_generator = TrafficGenerator::new()
  TrafficGenerator::start_http_traffic(traffic_generator, "http://example.com", 5) // 5个HTTP请求
  TrafficGenerator::start_https_traffic(traffic_generator, "https://example.com", 3) // 3个HTTPS请求
  
  // 等待捕获完成
  NetworkTrafficAnalyzer::wait_for_completion(traffic_analyzer)
  
  // 停止捕获
  NetworkTrafficAnalyzer::stop_capture(traffic_analyzer)
  
  // 分析捕获的流量
  let traffic_analysis = NetworkTrafficAnalyzer::analyze(traffic_analyzer)
  
  // 验证流量分析结果
  assert_true(TrafficAnalysis::has_total_packets(traffic_analysis))
  assert_true(TrafficAnalysis::has_total_bytes(traffic_analysis))
  assert_true(TrafficAnalysis::has_protocol_breakdown(traffic_analysis))
  assert_true(TrafficAnalysis::has_port_breakdown(traffic_analysis))
  assert_true(TrafficAnalysis::has_flow_analysis(traffic_analysis))
  
  // 验证总体统计
  let total_packets = TrafficAnalysis::get_total_packets(traffic_analysis)
  let total_bytes = TrafficAnalysis::get_total_bytes(traffic_analysis)
  
  assert_true(total_packets > 0)
  assert_true(total_bytes > 0)
  
  // 验证协议分布
  let protocol_breakdown = TrafficAnalysis::get_protocol_breakdown(traffic_analysis)
  assert_true(ProtocolBreakdown::has_protocol(protocol_breakdown, "TCP"))
  assert_true(ProtocolBreakdown::has_protocol(protocol_breakdown, "HTTP"))
  
  let tcp_percentage = ProtocolBreakdown::get_percentage(protocol_breakdown, "TCP")
  let http_percentage = ProtocolBreakdown::get_percentage(protocol_breakdown, "HTTP")
  
  assert_true(tcp_percentage > 0.0)
  assert_true(http_percentage > 0.0)
  assert_true(tcp_percentage + http_percentage <= 100.0)
  
  // 验证端口分布
  let port_breakdown = TrafficAnalysis::get_port_breakdown(traffic_analysis)
  assert_true(PortBreakdown::has_port(port_breakdown, 80))
  assert_true(PortBreakdown::has_port(port_breakdown, 443))
  
  let port_80_bytes = PortBreakdown::get_bytes(port_breakdown, 80)
  let port_443_bytes = PortBreakdown::get_bytes(port_breakdown, 443)
  
  assert_true(port_80_bytes > 0)
  assert_true(port_443_bytes > 0)
  
  // 验证流分析
  let flow_analysis = TrafficAnalysis::get_flow_analysis(traffic_analysis)
  assert_true(FlowAnalysis::has_flows(flow_analysis))
  
  let flows = FlowAnalysis::get_flows(flow_analysis)
  assert_true(Array::length(flows) > 0)
  
  // 验证流信息
  for flow in flows {
    assert_true(Flow::has_source_ip(flow))
    assert_true(Flow::has_destination_ip(flow))
    assert_true(Flow::has_source_port(flow))
    assert_true(Flow::has_destination_port(flow))
    assert_true(Flow::has_protocol(flow))
    assert_true(Flow::has_packet_count(flow))
    assert_true(Flow::has_byte_count(flow))
    assert_true(Flow::has_duration(flow))
    
    // 验证流数据的合理性
    let packet_count = Flow::get_packet_count(flow)
    let byte_count = Flow::get_byte_count(flow)
    let duration = Flow::get_duration(flow)
    
    assert_true(packet_count > 0)
    assert_true(byte_count > 0)
    assert_true(duration >= 0.0)
  }
  
  // 生成流量报告
  let traffic_report = NetworkTrafficAnalyzer::generate_report(traffic_analyzer)
  
  // 验证流量报告
  assert_true(TrafficReport::has_summary(traffic_report))
  assert_true(TrafficReport::has_charts(traffic_report))
  assert_true(TrafficReport::has_recommendations(traffic_report))
  
  // 验证报告摘要
  let summary = TrafficReport::get_summary(traffic_report)
  assert_true(ReportSummary::has_analysis_period(summary))
  assert_true(ReportSummary::has_top_protocols(summary))
  assert_true(ReportSummary::has_top_ports(summary))
  assert_true(ReportSummary::has_top_flows(summary))
  
  // 验证报告图表
  let charts = TrafficReport::get_charts(traffic_report)
  assert_true(Array::length(charts) >= 3) // 至少有协议分布、端口分布和流量趋势图表
  
  // 验证报告建议
  let recommendations = TrafficReport::get_recommendations(traffic_report)
  assert_true(Array::length(recommendations) >= 0)
}

test "网络性能基准测试" {
  // 测试网络性能基准测试功能
  let network_benchmark = NetworkBenchmark::new()
  
  // 配置基准测试
  let benchmark_config = BenchmarkConfig::new()
  BenchmarkConfig::set_targets(benchmark_config, [
    "api.example.com:443",
    "cdn.example.com:80",
    "db.example.com:5432"
  ])
  BenchmarkConfig::set_test_duration(benchmark_config, 30) // 30秒测试
  BenchmarkConfig::set_concurrent_connections(benchmark_config, 10)
  BenchmarkConfig::set_request_interval(benchmark_config, 100) // 100ms间隔
  BenchmarkConfig::set_payload_size(benchmark_config, 1024) // 1KB负载
  
  NetworkBenchmark::configure(network_benchmark, benchmark_config)
  
  // 开始基准测试
  NetworkBenchmark::start_benchmark(network_benchmark)
  
  // 等待测试完成
  NetworkBenchmark::wait_for_completion(network_benchmark)
  
  // 获取基准测试结果
  let benchmark_results = NetworkBenchmark::get_results(network_benchmark)
  
  // 验证基准测试结果
  assert_true(BenchmarkResults::has_target_results(benchmark_results))
  assert_true(BenchmarkResults::has_summary(benchmark_results))
  
  // 验证目标结果
  let target_results = BenchmarkResults::get_target_results(benchmark_results)
  assert_eq(Array::length(target_results), 3)
  
  for result in target_results {
    assert_true(TargetResult::has_target(result))
    assert_true(TargetResult::has_latency_stats(result))
    assert_true(TargetResult::has_throughput_stats(result))
    assert_true(TargetResult::has_error_rate(result))
    assert_true(TargetResult::has_connection_stats(result))
    
    // 验证延迟统计
    let latency_stats = TargetResult::get_latency_stats(result)
    assert_true(LatencyStats::has_min(latency_stats))
    assert_true(LatencyStats::has_max(latency_stats))
    assert_true(LatencyStats::has_avg(latency_stats))
    assert_true(LatencyStats::has_p50(latency_stats))
    assert_true(LatencyStats::has_p95(latency_stats))
    assert_true(LatencyStats::has_p99(latency_stats))
    
    let min_latency = LatencyStats::get_min(latency_stats)
    let max_latency = LatencyStats::get_max(latency_stats)
    let avg_latency = LatencyStats::get_avg(latency_stats)
    let p50_latency = LatencyStats::get_p50(latency_stats)
    let p95_latency = LatencyStats::get_p95(latency_stats)
    let p99_latency = LatencyStats::get_p99(latency_stats)
    
    assert_true(min_latency <= avg_latency)
    assert_true(avg_latency <= max_latency)
    assert_true(avg_latency <= p95_latency)
    assert_true(p95_latency <= p99_latency)
    assert_true(p99_latency <= max_latency)
    
    // 验证吞吐量统计
    let throughput_stats = TargetResult::get_throughput_stats(result)
    assert_true(ThroughputStats::has_requests_per_second(throughput_stats))
    assert_true(ThroughputStats::has_bytes_per_second(throughput_stats))
    
    let rps = ThroughputStats::get_requests_per_second(throughput_stats)
    let bps = ThroughputStats::get_bytes_per_second(throughput_stats)
    
    assert_true(rps > 0.0)
    assert_true(bps > 0.0)
    
    // 验证错误率
    let error_rate = TargetResult::get_error_rate(result)
    assert_true(error_rate >= 0.0 && error_rate <= 1.0)
    
    // 验证连接统计
    let connection_stats = TargetResult::get_connection_stats(result)
    assert_true(ConnectionStats::has_successful_connections(connection_stats))
    assert_true(ConnectionStats::has_failed_connections(connection_stats))
    assert_true(ConnectionStats::has_connection_time_stats(connection_stats))
    
    let successful_conns = ConnectionStats::get_successful_connections(connection_stats)
    let failed_conns = ConnectionStats::get_failed_connections(connection_stats)
    let total_conns = successful_conns + failed_conns
    
    assert_true(total_conns > 0)
    assert_true(successful_conns >= 0)
    assert_true(failed_conns >= 0)
  }
  
  // 验证基准测试摘要
  let summary = BenchmarkResults::get_summary(benchmark_results)
  assert_true(BenchmarkSummary::has_overall_latency_stats(summary))
  assert_true(BenchmarkSummary::has_overall_throughput_stats(summary))
  assert_true(BenchmarkSummary::has_overall_error_rate(summary))
  assert_true(BenchmarkSummary::has_best_performing_target(summary))
  assert_true(BenchmarkSummary::has_worst_performing_target(summary))
  
  // 验证最佳和最差性能目标
  let best_target = BenchmarkSummary::get_best_performing_target(summary)
  let worst_target = BenchmarkSummary::get_worst_performing_target(summary)
  
  assert_true(String::length(best_target) > 0)
  assert_true(String::length(worst_target) > 0)
  assert_ne(best_target, worst_target)
  
  // 生成基准测试报告
  let benchmark_report = NetworkBenchmark::generate_report(network_benchmark)
  
  // 验证基准测试报告
  assert_true(BenchmarkReport::has_executive_summary(benchmark_report))
  assert_true(BenchmarkReport::has_detailed_results(benchmark_report))
  assert_true(BenchmarkReport::has_performance_charts(benchmark_report))
  assert_true(BenchmarkReport::has_recommendations(benchmark_report))
  
  // 验证执行摘要
  let executive_summary = BenchmarkReport::get_executive_summary(benchmark_report)
  assert_true(ExecutiveSummary::has_test_overview(executive_summary))
  assert_true(ExecutiveSummary::has_key_findings(executive_summary))
  assert_true(ExecutiveSummary::has_performance_ranking(executive_summary))
  
  // 验证性能图表
  let performance_charts = BenchmarkReport::get_performance_charts(benchmark_report)
  assert_true(Array::length(performance_charts) >= 2) // 至少有延迟和吞吐量图表
  
  // 验证建议
  let recommendations = BenchmarkReport::get_recommendations(benchmark_report)
  assert_true(Array::length(recommendations) >= 0)
}