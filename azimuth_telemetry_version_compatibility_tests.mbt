// Azimuth 遥测数据版本兼容性测试
// 测试遥测数据的版本兼容性功能

// 测试1: 数据格式版本兼容性
test "数据格式版本兼容性测试" {
  // 创建版本兼容性管理器
  let compatibility_manager = CompatibilityManager::new()
  
  // 定义数据格式版本历史
  let format_versions = [
    {
      version: "1.0.0",
      release_date: "2022-01-01",
      description: "初始版本",
      schema: {
        span: {
          required_fields: ["trace_id", "span_id", "operation_name", "start_time", "end_time"],
          optional_fields: ["parent_span_id", "status", "attributes"],
          field_types: {
            trace_id: "string",
            span_id: "string",
            operation_name: "string",
            start_time: "integer",
            end_time: "integer",
            parent_span_id: "string",
            status: "string",
            attributes: "array"
          }
        }
      }
    },
    {
      version: "1.1.0",
      release_date: "2022-03-15",
      description: "添加服务名和资源属性",
      schema: {
        span: {
          required_fields: ["trace_id", "span_id", "operation_name", "start_time", "end_time", "service_name"],
          optional_fields: ["parent_span_id", "status", "attributes", "resource"],
          field_types: {
            trace_id: "string",
            span_id: "string",
            operation_name: "string",
            start_time: "integer",
            end_time: "integer",
            service_name: "string",
            parent_span_id: "string",
            status: "string",
            attributes: "array",
            resource: "object"
          }
        }
      }
    },
    {
      version: "1.2.0",
      release_date: "2022-06-01",
      description: "添加事件和链接",
      schema: {
        span: {
          required_fields: ["trace_id", "span_id", "operation_name", "start_time", "end_time", "service_name"],
          optional_fields: ["parent_span_id", "status", "attributes", "resource", "events", "links"],
          field_types: {
            trace_id: "string",
            span_id: "string",
            operation_name: "string",
            start_time: "integer",
            end_time: "integer",
            service_name: "string",
            parent_span_id: "string",
            status: "string",
            attributes: "array",
            resource: "object",
            events: "array",
            links: "array"
          }
        }
      }
    },
    {
      version: "2.0.0",
      release_date: "2022-09-15",
      description: "重大更新：重新组织字段结构",
      schema: {
        span: {
          required_fields: ["trace_id", "span_id", "name", "start_timestamp", "end_timestamp", "resource"],
          optional_fields: ["parent_span_id", "status", "attributes", "events", "links", "kind"],
          field_types: {
            trace_id: "string",
            span_id: "string",
            name: "string",          // 重命名自operation_name
            start_timestamp: "integer",  // 重命名自start_time
            end_timestamp: "integer",    // 重命名自end_time
            resource: "object",       // 从可选变为必填
            parent_span_id: "string",
            status: "string",
            attributes: "array",
            events: "array",
            links: "array",
            kind: "string"           // 新增字段
          }
        }
      }
    }
  ]
  
  // 注册格式版本
  for version in format_versions {
    let register_result = CompatibilityManager::register_format_version(compatibility_manager, version)
    assert_true(register_result.success)
  }
  
  // 创建v1.0.0格式的测试数据
  let v1_0_0_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995250,
    parent_span_id: Some("span-11111"),
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM orders")
    ]
  }
  
  // 测试v1.0.0到v1.1.0的升级
  let v1_0_to_v1_1_result = CompatibilityManager::upgrade_format(compatibility_manager, v1_0_0_data, "1.0.0", "1.1.0")
  
  // 验证升级结果
  assert_true(v1_0_to_v1_1_result.success)
  
  let v1_1_data = v1_0_to_v1_1_result.upgraded_data
  assert_eq(v1_1_data.trace_id, v1_0_0_data.trace_id)
  assert_eq(v1_1_data.span_id, v1_0_0_data.span_id)
  assert_eq(v1_1_data.operation_name, v1_0_0_data.operation_name)
  assert_eq(v1_1_data.start_time, v1_0_0_data.start_time)
  assert_eq(v1_1_data.end_time, v1_0_0_data.end_time)
  assert_eq(v1_1_data.parent_span_id, v1_0_0_data.parent_span_id)
  assert_eq(v1_1_data.status, v1_0_0_data.status)
  assert_eq(v1_1_data.attributes, v1_0_0_data.attributes)
  
  // 验证新增字段
  assert_eq(v1_1_data.service_name, "unknown")  // 默认值
  assert_eq(v1_1_data.resource, {})           // 默认空对象
  
  // 测试v1.1.0到v1.2.0的升级
  let v1_1_to_v1_2_result = CompatibilityManager::upgrade_format(compatibility_manager, v1_1_data, "1.1.0", "1.2.0")
  
  // 验证升级结果
  assert_true(v1_1_to_v1_2_result.success)
  
  let v1_2_data = v1_1_to_v1_2_result.upgraded_data
  assert_eq(v1_2_data.events, [])  // 默认空数组
  assert_eq(v1_2_data.links, [])   // 默认空数组
  
  // 测试v1.2.0到v2.0.0的升级（重大版本升级）
  let v1_2_to_v2_0_result = CompatibilityManager::upgrade_format(compatibility_manager, v1_2_data, "1.2.0", "2.0.0")
  
  // 验证重大版本升级结果
  assert_true(v1_2_to_v2_0_result.success)
  
  let v2_0_data = v1_2_to_v2_0_result.upgraded_data
  assert_eq(v2_0_data.trace_id, v1_2_data.trace_id)
  assert_eq(v2_0_data.span_id, v1_2_data.span_id)
  assert_eq(v2_0_data.name, v1_2_data.operation_name)  // 字段重命名
  assert_eq(v2_0_data.start_timestamp, v1_2_data.start_time)  // 字段重命名
  assert_eq(v2_0_data.end_timestamp, v1_2_data.end_time)    // 字段重命名
  assert_eq(v2_0_data.resource, v1_2_data.resource)  // 从可选变为必填
  assert_eq(v2_0_data.kind, "internal")  // 新增字段，默认值
  
  // 测试v2.0.0到v1.2.0的降级（重大版本降级）
  let v2_0_to_v1_2_result = CompatibilityManager::downgrade_format(compatibility_manager, v2_0_data, "2.0.0", "1.2.0")
  
  // 验证降级结果
  assert_true(v2_0_to_v1_2_result.success)
  
  let downgraded_v1_2_data = v2_0_to_v1_2_result.downgraded_data
  assert_eq(downgraded_v1_2_data.trace_id, v2_0_data.trace_id)
  assert_eq(downgraded_v1_2_data.span_id, v2_0_data.span_id)
  assert_eq(downgraded_v1_2_data.operation_name, v2_0_data.name)  // 字段重命名还原
  assert_eq(downgraded_v1_2_data.start_time, v2_0_data.start_timestamp)  // 字段重命名还原
  assert_eq(downgraded_v1_2_data.end_time, v2_0_data.end_timestamp)    // 字段重命名还原
  
  // 测试版本兼容性检查
  let compatibility_check = CompatibilityManager::check_compatibility(compatibility_manager, "1.1.0", "2.0.0")
  
  // 验证兼容性检查结果
  assert_false(compatibility_check.backward_compatible)  // v2.0.0不向后兼容v1.1.0
  assert_false(compatibility_check.forward_compatible)   // v1.1.0不向前兼容v2.0.0
  assert_true(compatibility_check.breaking_changes.length() > 0)
  
  let breaking_changes = compatibility_check.breaking_changes
  let operation_name_change = breaking_changes.find(fn(change) { 
    change.field == "operation_name" and change.type == "renamed" 
  })
  assert_true(operation_name_change != None)
  
  let service_name_change = breaking_changes.find(fn(change) { 
    change.field == "service_name" and change.type == "moved" 
  })
  assert_true(service_name_change != None)
  
  // 测试兼容性检查
  let minor_compatibility_check = CompatibilityManager::check_compatibility(compatibility_manager, "1.1.0", "1.2.0")
  
  // 验证小版本兼容性
  assert_true(minor_compatibility_check.backward_compatible)  // v1.2.0向后兼容v1.1.0
  assert_false(minor_compatibility_check.forward_compatible)   // v1.1.0不向前兼容v1.2.0
  assert_eq(minor_compatibility_check.breaking_changes.length(), 0)
  
  // 测试自动版本检测
  let version_detection_result = CompatibilityManager::detect_version(compatibility_manager, v1_2_data)
  
  // 验证版本检测结果
  assert_true(version_detection_result.success)
  assert_eq(version_detection_result.detected_version, "1.2.0")
  assert_true(version_detection_result.confidence > 0.8)
}

// 测试2: API版本兼容性
test "API版本兼容性测试" {
  // 创建API版本管理器
  let api_version_manager = ApiVersionManager::new()
  
  // 定义API版本
  let api_versions = [
    {
      version: "v1",
      status: "deprecated",
      deprecation_date: "2022-12-01",
      sunset_date: "2023-06-01",
      endpoints: [
        {
          path: "/api/v1/spans",
          method: "POST",
          request_schema: {
            type: "object",
            required: ["trace_id", "span_id", "operation_name"],
            properties: {
              trace_id: { type: "string" },
              span_id: { type: "string" },
              operation_name: { type: "string" },
              start_time: { type: "integer" },
              end_time: { type: "integer" }
            }
          },
          response_schema: {
            type: "object",
            properties: {
              success: { type: "boolean" },
              span_id: { type: "string" }
            }
          }
        },
        {
          path: "/api/v1/traces/{trace_id}/spans",
          method: "GET",
          response_schema: {
            type: "array",
            items: {
              type: "object",
              properties: {
                trace_id: { type: "string" },
                span_id: { type: "string" },
                operation_name: { type: "string" }
              }
            }
          }
        }
      ]
    },
    {
      version: "v2",
      status: "stable",
      deprecation_date: None,
      sunset_date: None,
      endpoints: [
        {
          path: "/api/v2/spans",
          method: "POST",
          request_schema: {
            type: "object",
            required: ["trace_id", "span_id", "name"],
            properties: {
              trace_id: { type: "string" },
              span_id: { type: "string" },
              name: { type: "string" },  // 重命名自operation_name
              start_timestamp: { type: "integer" },  // 重命名自start_time
              end_timestamp: { type: "integer" },    // 重命名自end_time
              resource: { type: "object" }  // 新增必填字段
            }
          },
          response_schema: {
            type: "object",
            properties: {
              success: { type: "boolean" },
              span_id: { type: "string" },
              warnings: { type: "array" }  // 新增字段
            }
          }
        },
        {
          path: "/api/v2/traces/{trace_id}/spans",
          method: "GET",
          parameters: [
            {
              name: "limit",
              type: "query",
              required: false
            },
            {
              name: "offset",
              type: "query",
              required: false
            }
          ],
          response_schema: {
            type: "object",
            properties: {
              spans: {
                type: "array",
                items: {
                  type: "object",
                  properties: {
                    trace_id: { type: "string" },
                    span_id: { type: "string" },
                    name: { type: "string" }
                  }
                }
              },
              total: { type: "integer" },  // 新增字段
              pagination: { type: "object" }  // 新增字段
            }
          }
        }
      ]
    },
    {
      version: "v3",
      status: "preview",
      deprecation_date: None,
      sunset_date: None,
      endpoints: [
        {
          path: "/api/v3/telemetry/spans",
          method: "POST",
          request_schema: {
            type: "object",
            required: ["resource_spans"],
            properties: {
              resource_spans: {
                type: "array",
                items: {
                  type: "object",
                  properties: {
                    resource: { type: "object" },
                    scope_spans: { type: "array" }
                  }
                }
              }
            }
          },
          response_schema: {
            type: "object",
            properties: {
              success: { type: "boolean" },
              accepted_spans: { type: "integer" }
            }
          }
        }
      ]
    }
  ]
  
  // 注册API版本
  for version in api_versions {
    let register_result = ApiVersionManager::register_api_version(api_version_manager, version)
    assert_true(register_result.success)
  }
  
  // 测试v1到v2的请求转换
  let v1_request = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995250
  }
  
  let v1_to_v2_result = ApiVersionManager::convert_request(api_version_manager, v1_request, "v1", "v2", "/api/v1/spans")
  
  // 验证请求转换结果
  assert_true(v1_to_v2_result.success)
  
  let v2_request = v1_to_v2_result.converted_request
  assert_eq(v2_request.trace_id, v1_request.trace_id)
  assert_eq(v2_request.span_id, v1_request.span_id)
  assert_eq(v2_request.name, v1_request.operation_name)  // 字段重命名
  assert_eq(v2_request.start_timestamp, v1_request.start_time)  // 字段重命名
  assert_eq(v2_request.end_timestamp, v1_request.end_time)    // 字段重命名
  assert_eq(v2_request.resource, {})  // 新增字段，默认空对象
  
  // 测试v2到v1的响应转换
  let v2_response = {
    success: true,
    span_id: "span-67890",
    warnings: ["deprecated field used"]
  }
  
  let v2_to_v1_result = ApiVersionManager::convert_response(api_version_manager, v2_response, "v2", "v1", "/api/v1/spans")
  
  // 验证响应转换结果
  assert_true(v2_to_v1_result.success)
  
  let v1_response = v2_to_v1_result.converted_response
  assert_eq(v1_response.success, v2_response.success)
  assert_eq(v1_response.span_id, v2_response.span_id)
  // warnings字段在v1中不存在，应该被忽略
  
  // 测试API版本协商
  let negotiation_result = ApiVersionManager::negotiate_version(api_version_manager, {
    client_versions: ["v1", "v2"],
    server_versions: ["v2", "v3"],
    preferred_version: None
  })
  
  // 验证版本协商结果
  assert_true(negotiation_result.success)
  assert_eq(negotiation_result.negotiated_version, "v2")  // 双方都支持的最新版本
  
  // 测试无共同版本的协商
  let no_common_result = ApiVersionManager::negotiate_version(api_version_manager, {
    client_versions: ["v1"],
    server_versions: ["v3"],
    preferred_version: None
  })
  
  // 验证无共同版本的协商结果
  assert_false(no_common_result.success)
  
  // 测试版本状态检查
  let v1_status = ApiVersionManager::get_version_status(api_version_manager, "v1")
  assert_eq(v1_status, "deprecated")
  
  let v2_status = ApiVersionManager::get_version_status(api_version_manager, "v2")
  assert_eq(v2_status, "stable")
  
  let v3_status = ApiVersionManager::get_version_status(api_version_manager, "v3")
  assert_eq(v3_status, "preview")
  
  // 测试弃用版本警告
  let deprecation_warning = ApiVersionManager::check_deprecation(api_version_manager, "v1")
  
  // 验证弃用警告
  assert_true(deprecation_warning.is_deprecated)
  assert_eq(deprecation_warning.deprecation_date, "2022-12-01")
  assert_eq(deprecation_warning.sunset_date, "2023-06-01")
  assert_true(deprecation_warning.recommended_migration_version == "v2")
  
  // 测试版本兼容性矩阵
  let compatibility_matrix = ApiVersionManager::get_compatibility_matrix(api_version_manager)
  
  // 验证兼容性矩阵
  assert_true(compatibility_matrix.matrix.contains_key("v1"))
  assert_true(compatibility_matrix.matrix.contains_key("v2"))
  assert_true(compatibility_matrix.matrix.contains_key("v3"))
  
  // v1和v2之间应该有兼容性映射
  let v1_to_v2_mapping = compatibility_matrix.matrix.get("v1").get("v2")
  assert_true(v1_to_v2_mapping != None)
  
  match v1_to_v2_mapping {
    Some(mapping) => {
      assert_true(mapping.request_converter != None)
      assert_true(mapping.response_converter != None)
    }
    None => assert_true(false)
  }
}

// 测试3: 数据库模式版本兼容性
test "数据库模式版本兼容性测试" {
  // 创建数据库版本管理器
  let db_version_manager = DatabaseVersionManager::new()
  
  // 定义数据库模式版本
  let schema_versions = [
    {
      version: "1.0.0",
      description: "初始模式",
      tables: {
        spans: {
          columns: [
            { name: "id", type: "UUID", primary_key: true, nullable: false },
            { name: "trace_id", type: "VARCHAR(255)", nullable: false },
            { name: "span_id", type: "VARCHAR(255)", nullable: false },
            { name: "operation_name", type: "VARCHAR(255)", nullable: false },
            { name: "start_time", type: "BIGINT", nullable: false },
            { name: "end_time", type: "BIGINT", nullable: false },
            { name: "parent_span_id", type: "VARCHAR(255)", nullable: true },
            { name: "status", type: "VARCHAR(50)", nullable: true },
            { name: "created_at", type: "TIMESTAMP", nullable: false }
          ],
          indexes: [
            { name: "idx_spans_trace_id", columns: ["trace_id"] },
            { name: "idx_spans_span_id", columns: ["span_id"] }
          ]
        }
      },
      migrations: []
    },
    {
      version: "1.1.0",
      description: "添加服务名和资源表",
      tables: {
        spans: {
          columns: [
            { name: "id", type: "UUID", primary_key: true, nullable: false },
            { name: "trace_id", type: "VARCHAR(255)", nullable: false },
            { name: "span_id", type: "VARCHAR(255)", nullable: false },
            { name: "operation_name", type: "VARCHAR(255)", nullable: false },
            { name: "start_time", type: "BIGINT", nullable: false },
            { name: "end_time", type: "BIGINT", nullable: false },
            { name: "parent_span_id", type: "VARCHAR(255)", nullable: true },
            { name: "status", type: "VARCHAR(50)", nullable: true },
            { name: "service_name", type: "VARCHAR(255)", nullable: false },  // 新增必填字段
            { name: "resource_id", type: "UUID", nullable: true },  // 新增外键
            { name: "created_at", type: "TIMESTAMP", nullable: false }
          ],
          indexes: [
            { name: "idx_spans_trace_id", columns: ["trace_id"] },
            { name: "idx_spans_span_id", columns: ["span_id"] },
            { name: "idx_spans_service_name", columns: ["service_name"] }  // 新增索引
          ]
        },
        resources: {
          columns: [
            { name: "id", type: "UUID", primary_key: true, nullable: false },
            { name: "attributes", type: "JSONB", nullable: true },
            { name: "created_at", type: "TIMESTAMP", nullable: false }
          ],
          indexes: []
        }
      },
      migrations: [
        {
          type: "add_column",
          table: "spans",
          column: { name: "service_name", type: "VARCHAR(255)", nullable: false, default: "'unknown'" }
        },
        {
          type: "add_column",
          table: "spans",
          column: { name: "resource_id", type: "UUID", nullable: true }
        },
        {
          type: "create_table",
          table: "resources",
          columns: [
            { name: "id", type: "UUID", primary_key: true, nullable: false },
            { name: "attributes", type: "JSONB", nullable: true },
            { name: "created_at", type: "TIMESTAMP", nullable: false }
          ]
        },
        {
          type: "add_index",
          table: "spans",
          index: { name: "idx_spans_service_name", columns: ["service_name"] }
        }
      ]
    },
    {
      version: "1.2.0",
      description: "添加事件和链接表",
      tables: {
        spans: {
          columns: [
            { name: "id", type: "UUID", primary_key: true, nullable: false },
            { name: "trace_id", type: "VARCHAR(255)", nullable: false },
            { name: "span_id", type: "VARCHAR(255)", nullable: false },
            { name: "operation_name", type: "VARCHAR(255)", nullable: false },
            { name: "start_time", type: "BIGINT", nullable: false },
            { name: "end_time", type: "BIGINT", nullable: false },
            { name: "parent_span_id", type: "VARCHAR(255)", nullable: true },
            { name: "status", type: "VARCHAR(50)", nullable: true },
            { name: "service_name", type: "VARCHAR(255)", nullable: false },
            { name: "resource_id", type: "UUID", nullable: true },
            { name: "duration_ms", type: "INTEGER", nullable: true },  // 新增计算字段
            { name: "created_at", type: "TIMESTAMP", nullable: false }
          ],
          indexes: [
            { name: "idx_spans_trace_id", columns: ["trace_id"] },
            { name: "idx_spans_span_id", columns: ["span_id"] },
            { name: "idx_spans_service_name", columns: ["service_name"] }
          ]
        },
        resources: {
          columns: [
            { name: "id", type: "UUID", primary_key: true, nullable: false },
            { name: "attributes", type: "JSONB", nullable: true },
            { name: "created_at", type: "TIMESTAMP", nullable: false }
          ],
          indexes: []
        },
        span_events: {
          columns: [
            { name: "id", type: "UUID", primary_key: true, nullable: false },
            { name: "span_id", type: "UUID", nullable: false },
            { name: "name", type: "VARCHAR(255)", nullable: false },
            { name: "timestamp", type: "BIGINT", nullable: false },
            { name: "attributes", type: "JSONB", nullable: true }
          ],
          indexes: [
            { name: "idx_span_events_span_id", columns: ["span_id"] }
          ]
        },
        span_links: {
          columns: [
            { name: "id", type: "UUID", primary_key: true, nullable: false },
            { name: "span_id", type: "UUID", nullable: false },
            { name: "linked_trace_id", type: "VARCHAR(255)", nullable: false },
            { name: "linked_span_id", type: "VARCHAR(255)", nullable: false },
            { name: "attributes", type: "JSONB", nullable: true }
          ],
          indexes: [
            { name: "idx_span_links_span_id", columns: ["span_id"] }
          ]
        }
      },
      migrations: [
        {
          type: "add_column",
          table: "spans",
          column: { name: "duration_ms", type: "INTEGER", nullable: true }
        },
        {
          type: "create_table",
          table: "span_events",
          columns: [
            { name: "id", type: "UUID", primary_key: true, nullable: false },
            { name: "span_id", type: "UUID", nullable: false },
            { name: "name", type: "VARCHAR(255)", nullable: false },
            { name: "timestamp", type: "BIGINT", nullable: false },
            { name: "attributes", type: "JSONB", nullable: true }
          ]
        },
        {
          type: "create_table",
          table: "span_links",
          columns: [
            { name: "id", type: "UUID", primary_key: true, nullable: false },
            { name: "span_id", type: "UUID", nullable: false },
            { name: "linked_trace_id", type: "VARCHAR(255)", nullable: false },
            { name: "linked_span_id", type: "VARCHAR(255)", nullable: false },
            { name: "attributes", type: "JSONB", nullable: true }
          ]
        },
        {
          type: "add_index",
          table: "span_events",
          index: { name: "idx_span_events_span_id", columns: ["span_id"] }
        },
        {
          type: "add_index",
          table: "span_links",
          index: { name: "idx_span_links_span_id", columns: ["span_id"] }
        }
      ]
    }
  ]
  
  // 注册数据库模式版本
  for version in schema_versions {
    let register_result = DatabaseVersionManager::register_schema_version(db_version_manager, version)
    assert_true(register_result.success)
  }
  
  // 测试迁移路径生成
  let migration_path_result = DatabaseVersionManager::generate_migration_path(db_version_manager, "1.0.0", "1.2.0")
  
  // 验证迁移路径
  assert_true(migration_path_result.success)
  
  let migration_path = migration_path_result.path
  assert_eq(migration_path.length(), 2)  // 1.0.0 -> 1.1.0 -> 1.2.0
  assert_eq(migration_path[0].from_version, "1.0.0")
  assert_eq(migration_path[0].to_version, "1.1.0")
  assert_eq(migration_path[1].from_version, "1.1.0")
  assert_eq(migration_path[1].to_version, "1.2.0")
  
  // 验证迁移步骤
  let v1_0_to_v1_1_migrations = migration_path[0].migrations
  assert_eq(v1_0_to_v1_1_migrations.length(), 4)
  
  let add_service_name_migration = v1_0_to_v1_1_migrations.find(fn(m) { 
    m.type == "add_column" and m.column.name == "service_name" 
  })
  assert_true(add_service_name_migration != None)
  
  let create_resources_migration = v1_0_to_v1_1_migrations.find(fn(m) { 
    m.type == "create_table" and m.table == "resources" 
  })
  assert_true(create_resources_migration != None)
  
  // 测试回滚路径生成
  let rollback_path_result = DatabaseVersionManager::generate_rollback_path(db_version_manager, "1.2.0", "1.0.0")
  
  // 验证回滚路径
  assert_true(rollback_path_result.success)
  
  let rollback_path = rollback_path_result.path
  assert_eq(rollback_path.length(), 2)  // 1.2.0 -> 1.1.0 -> 1.0.0
  assert_eq(rollback_path[0].from_version, "1.2.0")
  assert_eq(rollback_path[0].to_version, "1.1.0")
  assert_eq(rollback_path[1].from_version, "1.1.0")
  assert_eq(rollback_path[1].to_version, "1.0.0")
  
  // 验证回滚步骤
  let v1_2_to_v1_1_rollback = rollback_path[0].migrations
  assert_eq(v1_2_to_v1_1_rollback.length(), 4)
  
  let drop_span_events_migration = v1_2_to_v1_1_rollback.find(fn(m) { 
    m.type == "drop_table" and m.table == "span_events" 
  })
  assert_true(drop_span_events_migration != None)
  
  // 测试迁移验证
  let migration_validation_result = DatabaseVersionManager::validate_migrations(db_version_manager, migration_path)
  
  // 验证迁移验证结果
  assert_true(migration_validation_result.valid)
  assert_eq(migration_validation_result.errors.length(), 0)
  
  // 测试数据兼容性检查
  let data_compatibility_result = DatabaseVersionManager::check_data_compatibility(db_version_manager, {
    from_version: "1.0.0",
    to_version: "1.2.0",
    sample_data: [
      {
        table: "spans",
        data: {
          trace_id: "trace-12345",
          span_id: "span-67890",
          operation_name: "database.query",
          start_time: 1640995200,
          end_time: 1640995250
        }
      }
    ]
  })
  
  // 验证数据兼容性
  assert_true(data_compatibility_result.compatible)
  assert_eq(data_compatibility_result.transformations.length(), 2)  // 需要添加service_name和duration_ms
  
  // 测试破坏性变更检测
  let breaking_changes_result = DatabaseVersionManager::detect_breaking_changes(db_version_manager, "1.0.0", "1.2.0")
  
  // 验证破坏性变更
  assert_true(breaking_changes_result.has_breaking_changes)
  
  let service_name_change = breaking_changes_result.changes.find(fn(change) { 
    change.table == "spans" and change.type == "add_required_column" and change.column == "service_name" 
  })
  assert_true(service_name_change != None)
  
  // 测试迁移执行模拟
  let migration_execution_result = DatabaseVersionManager::simulate_migration(db_version_manager, {
    from_version: "1.0.0",
    to_version: "1.1.0",
    dry_run: true
  })
  
  // 验证迁移执行模拟
  assert_true(migration_execution_result.success)
  assert_eq(migration_execution_result.executed_migrations.length(), 4)
  assert_true(migration_execution_result.execution_time_ms > 0)
  
  // 测试模式差异比较
  let schema_diff_result = DatabaseVersionManager::compare_schemas(db_version_manager, "1.0.0", "1.2.0")
  
  // 验证模式差异
  assert_true(schema_diff_result.added_tables.length() > 0)  // span_events, span_links
  assert_true(schema_diff_result.added_columns.length() > 0)  // service_name, resource_id, duration_ms
  assert_true(schema_diff_result.added_indexes.length() > 0)
}

// 测试4: 客户端库版本兼容性
test "客户端库版本兼容性测试" {
  // 创建客户端版本管理器
  let client_version_manager = ClientVersionManager::new()
  
  // 定义客户端库版本
  let client_versions = [
    {
      version: "1.0.0",
      language: "java",
      release_date: "2022-01-15",
      compatibility: {
        min_server_version: "1.0.0",
        max_server_version: "1.2.0",
        supported_formats: ["json", "protobuf"],
        deprecated_features: [],
        new_features: ["basic_tracing", "metrics"]
      }
    },
    {
      version: "1.1.0",
      language: "java",
      release_date: "2022-03-15",
      compatibility: {
        min_server_version: "1.0.0",
        max_server_version: "1.3.0",
        supported_formats: ["json", "protobuf", "otlp"],
        deprecated_features: ["legacy_metrics"],
        new_features: ["batch_processing", "compression"]
      }
    },
    {
      version: "2.0.0",
      language: "java",
      release_date: "2022-09-01",
      compatibility: {
        min_server_version: "2.0.0",
        max_server_version: "2.1.0",
        supported_formats: ["otlp", "protobuf"],
        deprecated_features: ["json_format", "legacy_api"],
        new_features: ["auto_instrumentation", "async_tracing"]
      }
    },
    {
      version: "1.0.0",
      language: "python",
      release_date: "2022-02-01",
      compatibility: {
        min_server_version: "1.0.0",
        max_server_version: "1.2.0",
        supported_formats: ["json", "protobuf"],
        deprecated_features: [],
        new_features: ["basic_tracing", "metrics"]
      }
    },
    {
      version: "1.2.0",
      language: "python",
      release_date: "2022-07-15",
      compatibility: {
        min_server_version: "1.1.0",
        max_server_version: "2.0.0",
        supported_formats: ["json", "protobuf", "otlp"],
        deprecated_features: ["sync_only"],
        new_features: ["async_support", "context_propagation"]
      }
    }
  ]
  
  // 注册客户端版本
  for version in client_versions {
    let register_result = ClientVersionManager::register_client_version(client_version_manager, version)
    assert_true(register_result.success)
  }
  
  // 测试客户端-服务器兼容性检查
  let java_1_0_compatibility = ClientVersionManager::check_server_compatibility(client_version_manager, {
    client_version: "1.0.0",
    language: "java",
    server_version: "1.1.0"
  })
  
  // 验证Java 1.0客户端与服务器1.1的兼容性
  assert_true(java_1_0_compatibility.compatible)
  assert_true(java_1_0_compatibility.supported_formats.contains("json"))
  assert_true(java_1_0_compatibility.supported_formats.contains("protobuf"))
  assert_false(java_1_0_compatibility.supported_formats.contains("otlp"))  // 1.0不支持otlp
  
  // 测试Java 2.0客户端与服务器1.2的兼容性
  let java_2_0_compatibility = ClientVersionManager::check_server_compatibility(client_version_manager, {
    client_version: "2.0.0",
    language: "java",
    server_version: "1.2.0"
  })
  
  // 验证Java 2.0客户端与服务器1.2的不兼容性
  assert_false(java_2_0_compatibility.compatible)
  assert_true(java_2_0_compatibility.reason.contains("server_version"))
  
  // 测试Python 1.2客户端与服务器2.0的兼容性
  let python_1_2_compatibility = ClientVersionManager::check_server_compatibility(client_version_manager, {
    client_version: "1.2.0",
    language: "python",
    server_version: "2.0.0"
  })
  
  // 验证Python 1.2客户端与服务器2.0的兼容性
  assert_true(python_1_2_compatibility.compatible)
  assert_true(python_1_2_compatibility.supported_formats.contains("otlp"))
  
  // 测试推荐客户端版本
  let recommendation_result = ClientVersionManager::recommend_client_version(client_version_manager, {
    language: "java",
    server_version: "1.2.0",
    features: ["batch_processing", "compression"]
  })
  
  // 验证推荐结果
  assert_true(recommendation_result.success)
  assert_eq(recommendation_result.recommended_version, "1.1.0")  // 支持所需功能的最新兼容版本
  
  // 测试功能兼容性检查
  let feature_compatibility_result = ClientVersionManager::check_feature_compatibility(client_version_manager, {
    client_version: "1.0.0",
    language: "java",
    features: ["basic_tracing", "batch_processing", "async_tracing"]
  })
  
  // 验证功能兼容性
  assert_true(feature_compatibility_result.supported_features.contains("basic_tracing"))
  assert_false(feature_compatibility_result.unsupported_features.contains("basic_tracing"))
  
  assert_false(feature_compatibility_result.supported_features.contains("batch_processing"))  // 1.0不支持
  assert_true(feature_compatibility_result.unsupported_features.contains("batch_processing"))
  
  assert_false(feature_compatibility_result.supported_features.contains("async_tracing"))     // 1.0不支持
  assert_true(feature_compatibility_result.unsupported_features.contains("async_tracing"))
  
  // 测试弃用功能警告
  let deprecation_warning_result = ClientVersionManager::check_deprecated_features(client_version_manager, {
    client_version: "1.1.0",
    language: "java"
  })
  
  // 验证弃用功能警告
  assert_true(deprecation_warning_result.has_deprecated_features)
  assert_true(deprecation_warning_result.deprecated_features.contains("legacy_metrics"))
  
  // 测试升级路径
  let upgrade_path_result = ClientVersionManager::get_upgrade_path(client_version_manager, {
    language: "java",
    from_version: "1.0.0",
    to_version: "2.0.0"
  })
  
  // 验证升级路径
  assert_true(upgrade_path_result.success)
  assert_eq(upgrade_path_result.path.length(), 2)  // 1.0.0 -> 1.1.0 -> 2.0.0
  assert_eq(upgrade_path_result.path[0], "1.1.0")
  assert_eq(upgrade_path_result.path[1], "2.0.0")
  
  // 验证升级建议
  assert_true(upgrade_path_result.breaking_changes.length() > 0)
  assert_true(upgrade_path_result.new_features.length() > 0)
  
  // 测试跨语言兼容性
  let cross_language_result = ClientVersionManager::check_cross_language_compatibility(client_version_manager, {
    client1: { language: "java", version: "1.1.0" },
    client2: { language: "python", version: "1.2.0" },
    server_version: "1.2.0"
  })
  
  // 验证跨语言兼容性
  assert_true(cross_language_result.compatible)
  assert_true(cross_language_result.common_formats.length() > 0)
  assert_true(cross_language_result.common_formats.contains("protobuf"))
  assert_true(cross_language_result.common_formats.contains("otlp"))
  
  // 测试版本矩阵生成
  let version_matrix_result = ClientVersionManager::generate_compatibility_matrix(client_version_manager, {
    languages: ["java", "python"],
    server_versions: ["1.0.0", "1.1.0", "1.2.0", "2.0.0"]
  })
  
  // 验证版本矩阵
  assert_true(version_matrix_result.success)
  
  let matrix = version_matrix_result.matrix
  assert_true(matrix.contains_key("java"))
  assert_true(matrix.contains_key("python"))
  
  let java_matrix = matrix.get("java")
  assert_true(java_matrix.contains_key("1.0.0"))
  assert_true(java_matrix.contains_key("2.0.0"))
  
  // 验证Java 1.0与服务器1.0的兼容性
  let java_1_0_server_1_0 = java_matrix.get("1.0.0").get("1.0.0")
  assert_true(java_1_0_server_1_0.compatible)
  
  // 验证Java 2.0与服务器1.0的不兼容性
  let java_2_0_server_1_0 = java_matrix.get("2.0.0").get("1.0.0")
  assert_false(java_2_0_server_1_0.compatible)
}

// 测试5: 协议版本兼容性
test "协议版本兼容性测试" {
  // 创建协议版本管理器
  let protocol_version_manager = ProtocolVersionManager::new()
  
  // 定义协议版本
  let protocol_versions = [
    {
      version: "0.5.0",
      status: "deprecated",
      description: "早期实验版本",
      wire_format: {
        magic_bytes: [0x54, 0x45, 0x4C],  // "TEL"
        version: 1,
        flags: 0
      },
      message_types: [
        {
          name: "Span",
          id: 1,
          fields: [
            { id: 1, name: "trace_id", type: "string", required: true },
            { id: 2, name: "span_id", type: "string", required: true },
            { id: 3, name: "operation_name", type: "string", required: true },
            { id: 4, name: "start_time", type: "uint64", required: true },
            { id: 5, name: "end_time", type: "uint64", required: true }
          ]
        }
      ]
    },
    {
      version: "1.0.0",
      status: "stable",
      description: "第一个稳定版本",
      wire_format: {
        magic_bytes: [0x54, 0x45, 0x4C],  // "TEL"
        version: 2,
        flags: 0
      },
      message_types: [
        {
          name: "Span",
          id: 1,
          fields: [
            { id: 1, name: "trace_id", type: "string", required: true },
            { id: 2, name: "span_id", type: "string", required: true },
            { id: 3, name: "operation_name", type: "string", required: true },
            { id: 4, name: "start_time", type: "uint64", required: true },
            { id: 5, name: "end_time", type: "uint64", required: true },
            { id: 6, name: "service_name", type: "string", required: true }
          ]
        },
        {
          name: "Metric",
          id: 2,
          fields: [
            { id: 1, name: "name", type: "string", required: true },
            { id: 2, name: "value", type: "double", required: true },
            { id: 3, name: "tags", type: "map", required: false }
          ]
        }
      ]
    },
    {
      version: "1.1.0",
      status: "stable",
      description: "添加压缩支持",
      wire_format: {
        magic_bytes: [0x54, 0x45, 0x4C],  // "TEL"
        version: 2,
        flags: 1  // 启用压缩标志
      },
      message_types: [
        {
          name: "Span",
          id: 1,
          fields: [
            { id: 1, name: "trace_id", type: "string", required: true },
            { id: 2, name: "span_id", type: "string", required: true },
            { id: 3, name: "operation_name", type: "string", required: true },
            { id: 4, name: "start_time", type: "uint64", required: true },
            { id: 5, name: "end_time", type: "uint64", required: true },
            { id: 6, name: "service_name", type: "string", required: true }
          ]
        },
        {
          name: "Metric",
          id: 2,
          fields: [
            { id: 1, name: "name", type: "string", required: true },
            { id: 2, name: "value", type: "double", required: true },
            { id: 3, name: "tags", type: "map", required: false }
          ]
        }
      ]
    },
    {
      version: "2.0.0",
      status: "stable",
      description: "重新设计协议，支持流式传输",
      wire_format: {
        magic_bytes: [0x54, 0x45, 0x4C],  // "TEL"
        version: 3,
        flags: 0
      },
      message_types: [
        {
          name: "ResourceSpans",
          id: 1,
          fields: [
            { id: 1, name: "resource", type: "Resource", required: true },
            { id: 2, name: "scope_spans", type: "repeated ScopeSpans", required: true }
          ]
        },
        {
          name: "Resource",
          id: 2,
          fields: [
            { id: 1, name: "attributes", type: "map", required: true }
          ]
        },
        {
          name: "ScopeSpans",
          id: 3,
          fields: [
            { id: 1, name: "scope", type: "InstrumentationScope", required: true },
            { id: 2, name: "spans", type: "repeated Span", required: true }
          ]
        }
      ]
    }
  ]
  
  // 注册协议版本
  for version in protocol_versions {
    let register_result = ProtocolVersionManager::register_protocol_version(protocol_version_manager, version)
    assert_true(register_result.success)
  }
  
  // 测试协议消息编码
  let v1_0_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "payment-service"
  }
  
  let v1_0_encode_result = ProtocolVersionManager::encode_message(protocol_version_manager, v1_0_span, "1.0.0", "Span")
  
  // 验证编码结果
  assert_true(v1_0_encode_result.success)
  assert_true(v1_0_encode_result.encoded_data.length() > 0)
  
  // 测试协议消息解码
  let v1_0_decode_result = ProtocolVersionManager::decode_message(protocol_version_manager, v1_0_encode_result.encoded_data, "1.0.0")
  
  // 验证解码结果
  assert_true(v1_0_decode_result.success)
  
  let decoded_span = v1_0_decode_result.decoded_message
  assert_eq(decoded_span.trace_id, v1_0_span.trace_id)
  assert_eq(decoded_span.span_id, v1_0_span.span_id)
  assert_eq(decoded_span.operation_name, v1_0_span.operation_name)
  assert_eq(decoded_span.start_time, v1_0_span.start_time)
  assert_eq(decoded_span.end_time, v1_0_span.end_time)
  assert_eq(decoded_span.service_name, v1_0_span.service_name)
  
  // 测试协议版本协商
  let negotiation_result = ProtocolVersionManager::negotiate_protocol(protocol_version_manager, {
    client_versions: ["1.0.0", "1.1.0"],
    server_versions: ["1.1.0", "2.0.0"],
    capabilities: ["compression"]
  })
  
  // 验证协议协商结果
  assert_true(negotiation_result.success)
  assert_eq(negotiation_result.negotiated_version, "1.1.0")  // 双方都支持的最新版本
  assert_true(negotiation_result.supported_capabilities.contains("compression"))
  
  // 测试协议转换
  let v1_0_to_v2_0_result = ProtocolVersionManager::convert_message(protocol_version_manager, {
    message: v1_0_span,
    from_version: "1.0.0",
    to_version: "2.0.0",
    message_type: "Span"
  })
  
  // 验证协议转换结果
  assert_true(v1_0_to_v2_0_result.success)
  
  let v2_0_span = v1_0_to_v2_0_result.converted_message
  assert_eq(v2_0_span.resource_spans[0].scope_spans[0].spans[0].trace_id, v1_0_span.trace_id)
  assert_eq(v2_0_span.resource_spans[0].scope_spans[0].spans[0].span_id, v1_0_span.span_id)
  
  // 测试协议兼容性检查
  let compatibility_check = ProtocolVersionManager::check_compatibility(protocol_version_manager, "1.0.0", "2.0.0")
  
  // 验证协议兼容性
  assert_false(compatibility_check.backward_compatible)  // v2.0.0不向后兼容v1.0.0
  assert_false(compatibility_check.forward_compatible)   // v1.0.0不向前兼容v2.0.0
  assert_true(compatibility_check.breaking_changes.length() > 0)
  
  // 测试协议特性检测
  let features_result = ProtocolVersionManager::get_protocol_features(protocol_version_manager, "1.1.0")
  
  // 验证协议特性
  assert_true(features_result.features.contains("compression"))
  assert_true(features_result.features.contains("batch_processing"))
  assert_false(features_result.features.contains("streaming"))  // 1.1.0不支持流式传输
  
  // 测试协议错误处理
  let invalid_data = [0xFF, 0xFF, 0xFF, 0xFF]  // 无效的协议数据
  let error_result = ProtocolVersionManager::decode_message(protocol_version_manager, invalid_data, "1.0.0")
  
  // 验证错误处理
  assert_false(error_result.success)
  assert_true(error_result.error.contains("invalid") or error_result.error.contains("magic"))
  
  // 测试协议性能
  let large_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation_name: "large.operation",
    start_time: 1640995200,
    end_time: 1640995250,
    service_name: "large-service",
    attributes: Array::range(0, 100).map(fn(i) { 
      ("attribute." + i.to_string(), "value." + i.to_string()) 
    })
  }
  
  let performance_start = Time::now()
  let large_encode_result = ProtocolVersionManager::encode_message(protocol_version_manager, large_span, "1.1.0", "Span")
  let large_decode_result = ProtocolVersionManager::decode_message(protocol_version_manager, large_encode_result.encoded_data, "1.1.0")
  let performance_time = Time::now() - performance_start
  
  // 验证性能
  assert_true(large_encode_result.success)
  assert_true(large_decode_result.success)
  assert_true(performance_time < 1000)  // 应该在1秒内完成
  
  // 测试协议版本矩阵
  let version_matrix = ProtocolVersionManager::get_version_matrix(protocol_version_manager)
  
  // 验证版本矩阵
  assert_true(version_matrix.matrix.contains_key("1.0.0"))
  assert_true(version_matrix.matrix.contains_key("2.0.0"))
  
  let v1_0_to_v2_0 = version_matrix.matrix.get("1.0.0").get("2.0.0")
  assert_false(v1_0_to_v2_0.compatible)
  assert_true(v1_0_to_v2_0.converter != None)
  
  let v1_0_to_v1_1 = version_matrix.matrix.get("1.0.0").get("1.1.0")
  assert_true(v1_0_to_v1_1.compatible)
}