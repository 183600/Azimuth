// Azimuth Multidimensional Attribute Query Tests
// 多维属性查询测试用例

test "complex attribute filtering and querying" {
  // 测试复杂属性过滤和查询
  let query_filter = @azimuth.QueryFilter {
    attribute_filters : [
      @azimuth.AttributeFilter {
        attribute : "service.name",
        operator : @azimuth.FilterOperator::Equals,
        value : @azimuth.StringValue("payment-service")
      },
      @azimuth.AttributeFilter {
        attribute : "http.status_code",
        operator : @azimuth.FilterOperator::In,
        value : @azimuth.ArrayValue([@azimuth.IntValue(200), @azimuth.IntValue(201), @azimuth.IntValue(202)])
      },
      @azimuth.AttributeFilter {
        attribute : "duration_ms",
        operator : @azimuth.FilterOperator::GreaterThan,
        value : @azimuth.IntValue(100)
      },
      @azimuth.AttributeFilter {
        attribute : "cloud.region",
        operator : @azimuth.FilterOperator::In,
        value : @azimuth.ArrayValue([
          @azimuth.StringValue("us-west-2"),
          @azimuth.StringValue("us-east-1"),
          @azimuth.StringValue("eu-west-1")
        ])
      }
    ],
    time_range : @azimuth.TimeRange {
      start_time : 1640995200000L, // 2022-01-01 00:00:00
      end_time : 1640995260000L     // 2022-01-01 00:10:00
    },
    limit : 1000,
    offset : 0
  }
  
  // 验证查询过滤器
  assert_eq(query_filter.attribute_filters.length(), 4)
  assert_eq(query_filter.time_range.start_time, 1640995200000L)
  assert_eq(query_filter.time_range.end_time, 1640995260000L)
  assert_eq(query_filter.limit, 1000)
  assert_eq(query_filter.offset, 0)
  
  // 验证服务名称过滤器
  let service_filter = query_filter.attribute_filters[0]
  assert_eq(service_filter.attribute, "service.name")
  match service_filter.operator {
    @azimuth.FilterOperator::Equals => assert_true(true)
    _ => assert_true(false)
  }
  match service_filter.value {
    @azimuth.StringValue(v) => assert_eq(v, "payment-service")
    _ => assert_true(false)
  }
  
  // 验证状态码过滤器
  let status_filter = query_filter.attribute_filters[1]
  assert_eq(status_filter.attribute, "http.status_code")
  match status_filter.operator {
    @azimuth.FilterOperator::In => assert_true(true)
    _ => assert_true(false)
  }
  match status_filter.value {
    @azimuth.ArrayValue(values) => {
      assert_eq(values.length(), 3)
      match values[0] {
        @azimuth.IntValue(v) => assert_eq(v, 200)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

test "aggregation queries with grouping" {
  // 测试带分组的聚合查询
  let aggregation_query = @azimuth.AggregationQuery {
    metric_name : "http.request.duration",
    aggregation_functions : [
      @azimuth.AggregationFunctionSpec {
        function : @azimuth.AggregationFunction::Avg,
        alias : "avg_duration"
      },
      @azimuth.AggregationFunctionSpec {
        function : @azimuth.AggregationFunction::P95,
        alias : "p95_duration"
      },
      @azimuth.AggregationFunctionSpec {
        function : @azimuth.AggregationFunction::Count,
        alias : "request_count"
      }
    ],
    group_by_attributes : [
      "service.name",
      "http.method",
      "http.status_code",
      "cloud.region"
    ],
    time_bucket : @azimuth.TimeBucket {
      bucket_size_ms : 60000, // 1分钟桶
      bucket_count : 10
    },
    time_range : @azimuth.TimeRange {
      start_time : 1640995200000L,
      end_time : 1640995800000L
    }
  }
  
  // 验证聚合查询
  assert_eq(aggregation_query.metric_name, "http.request.duration")
  assert_eq(aggregation_query.aggregation_functions.length(), 3)
  assert_eq(aggregation_query.group_by_attributes.length(), 4)
  assert_eq(aggregation_query.time_bucket.bucket_size_ms, 60000)
  assert_eq(aggregation_query.time_bucket.bucket_count, 10)
  
  // 验证聚合函数
  let avg_func = aggregation_query.aggregation_functions[0]
  match avg_func.function {
    @azimuth.AggregationFunction::Avg => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(avg_func.alias, "avg_duration")
  
  let p95_func = aggregation_query.aggregation_functions[1]
  match p95_func.function {
    @azimuth.AggregationFunction::P95 => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(p95_func.alias, "p95_duration")
  
  let count_func = aggregation_query.aggregation_functions[2]
  match count_func.function {
    @azimuth.AggregationFunction::Count => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(count_func.alias, "request_count")
}

test "nested attribute queries" {
  // 测试嵌套属性查询
  let nested_query = @azimuth.NestedAttributeQuery {
    base_attribute : "resource",
    nested_attributes : [
      @azimuth.NestedAttributeSpec {
        path : "k8s.pod.name",
        alias : "pod_name"
      },
      @azimuth.NestedAttributeSpec {
        path : "k8s.namespace",
        alias : "namespace"
      },
      @azimuth.NestedAttributeSpec {
        path : "k8s.deployment.name",
        alias : "deployment_name"
      }
    ],
    filters : [
      @azimuth.NestedAttributeFilter {
        path : "k8s.namespace",
        operator : @azimuth.FilterOperator::Equals,
        value : @azimuth.StringValue("production")
      },
      @azimuth.NestedAttributeFilter {
        path : "k8s.pod.name",
        operator : @azimuth.FilterOperator::StartsWith,
        value : @azimuth.StringValue("payment-service")
      }
    ]
  }
  
  // 验证嵌套查询
  assert_eq(nested_query.base_attribute, "resource")
  assert_eq(nested_query.nested_attributes.length(), 3)
  assert_eq(nested_query.filters.length(), 2)
  
  // 验证嵌套属性规范
  let pod_spec = nested_query.nested_attributes[0]
  assert_eq(pod_spec.path, "k8s.pod.name")
  assert_eq(pod_spec.alias, "pod_name")
  
  let namespace_spec = nested_query.nested_attributes[1]
  assert_eq(namespace_spec.path, "k8s.namespace")
  assert_eq(namespace_spec.alias, "namespace")
  
  // 验证嵌套过滤器
  let namespace_filter = nested_query.filters[0]
  assert_eq(namespace_filter.path, "k8s.namespace")
  match namespace_filter.operator {
    @azimuth.FilterOperator::Equals => assert_true(true)
    _ => assert_true(false)
  }
  match namespace_filter.value {
    @azimuth.StringValue(v) => assert_eq(v, "production")
    _ => assert_true(false)
  }
  
  let pod_filter = nested_query.filters[1]
  assert_eq(pod_filter.path, "k8s.pod.name")
  match pod_filter.operator {
    @azimuth.FilterOperator::StartsWith => assert_true(true)
    _ => assert_true(false)
  }
  match pod_filter.value {
    @azimuth.StringValue(v) => assert_eq(v, "payment-service")
    _ => assert_true(false)
  }
}

test "time series data queries" {
  // 测试时间序列数据查询
  let time_series_query = @azimuth.TimeSeriesQuery {
    metric_name : "system.cpu.usage",
    time_range : @azimuth.TimeRange {
      start_time : 1640995200000L,
      end_time : 1640995800000L
    },
    granularity : @azimuth.TimeGranularity::Minute,
    interpolation : @azimuth.InterpolationMethod::Linear,
    filters : [
      @azimuth.AttributeFilter {
        attribute : "service.name",
        operator : @azimuth.FilterOperator::Equals,
        value : @azimuth.StringValue("payment-service")
      },
      @azimuth.AttributeFilter {
        attribute : "host.name",
        operator : @azimuth.FilterOperator::In,
        value : @azimuth.ArrayValue([
          @azimuth.StringValue("payment-1"),
          @azimuth.StringValue("payment-2"),
          @azimuth.StringValue("payment-3")
        ])
      }
    ],
    aggregations : [
      @azimuth.AggregationFunctionSpec {
        function : @azimuth.AggregationFunction::Avg,
        alias : "avg_cpu"
      },
      @azimuth.AggregationFunctionSpec {
        function : @azimuth.AggregationFunction::Max,
        alias : "max_cpu"
      }
    ]
  }
  
  // 验证时间序列查询
  assert_eq(time_series_query.metric_name, "system.cpu.usage")
  assert_eq(time_series_query.granularity, @azimuth.TimeGranularity::Minute)
  assert_eq(time_series_query.interpolation, @azimuth.InterpolationMethod::Linear)
  assert_eq(time_series_query.filters.length(), 2)
  assert_eq(time_series_query.aggregations.length(), 2)
  
  // 验证时间范围
  assert_eq(time_series_query.time_range.start_time, 1640995200000L)
  assert_eq(time_series_query.time_range.end_time, 1640995800000L)
  
  // 验证过滤器
  let service_filter = time_series_query.filters[0]
  assert_eq(service_filter.attribute, "service.name")
  match service_filter.value {
    @azimuth.StringValue(v) => assert_eq(v, "payment-service")
    _ => assert_true(false)
  }
  
  let host_filter = time_series_query.filters[1]
  assert_eq(host_filter.attribute, "host.name")
  match host_filter.value {
    @azimuth.ArrayValue(hosts) => {
      assert_eq(hosts.length(), 3)
      match hosts[0] {
        @azimuth.StringValue(v) => assert_eq(v, "payment-1")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

test "facet queries and analysis" {
  // 测试分面查询和分析
  let facet_query = @azimuth.FacetQuery {
    base_query : @azimuth.QueryFilter {
      attribute_filters : [
        @azimuth.AttributeFilter {
          attribute : "service.name",
          operator : @azimuth.FilterOperator::Equals,
          value : @azimuth.StringValue("payment-service")
        }
      ],
      time_range : @azimuth.TimeRange {
        start_time : 1640995200000L,
        end_time : 1640995800000L
      },
      limit : 1000,
      offset : 0
    },
    facet_attributes : [
      @azimuth.FacetAttributeSpec {
        attribute : "http.method",
        max_values : 10,
        sort_by : @azimuth.FacetSortBy::Count,
        sort_order : @azimuth.SortOrder::Descending
      },
      @azimuth.FacetAttributeSpec {
        attribute : "http.status_code",
        max_values : 20,
        sort_by : @azimuth.FacetSortBy::Count,
        sort_order : @azimuth.SortOrder::Descending
      },
      @azimuth.FacetAttributeSpec {
        attribute : "cloud.region",
        max_values : 15,
        sort_by : @azimuth.FacetSortBy::Alphabetical,
        sort_order : @azimuth.SortOrder::Ascending
      }
    ]
  }
  
  // 验证分面查询
  assert_eq(facet_query.base_query.attribute_filters.length(), 1)
  assert_eq(facet_query.facet_attributes.length(), 3)
  
  // 验证基础查询
  let base_service_filter = facet_query.base_query.attribute_filters[0]
  assert_eq(base_service_filter.attribute, "service.name")
  match base_service_filter.value {
    @azimuth.StringValue(v) => assert_eq(v, "payment-service")
    _ => assert_true(false)
  }
  
  // 验证HTTP方法分面
  let method_facet = facet_query.facet_attributes[0]
  assert_eq(method_facet.attribute, "http.method")
  assert_eq(method_facet.max_values, 10)
  match method_facet.sort_by {
    @azimuth.FacetSortBy::Count => assert_true(true)
    _ => assert_true(false)
  }
  match method_facet.sort_order {
    @azimuth.SortOrder::Descending => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证状态码分面
  let status_facet = facet_query.facet_attributes[1]
  assert_eq(status_facet.attribute, "http.status_code")
  assert_eq(status_facet.max_values, 20)
  match status_facet.sort_by {
    @azimuth.FacetSortBy::Count => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证区域分面
  let region_facet = facet_query.facet_attributes[2]
  assert_eq(region_facet.attribute, "cloud.region")
  assert_eq(region_facet.max_values, 15)
  match region_facet.sort_by {
    @azimuth.FacetSortBy::Alphabetical => assert_true(true)
    _ => assert_true(false)
  }
  match region_facet.sort_order {
    @azimuth.SortOrder::Ascending => assert_true(true)
    _ => assert_true(false)
  }
}