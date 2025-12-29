test "propagation_edge_cases" {
  // 测试传播的边界情况
  
  // 测试空carrier
  let empty_carrier = MapCarrier::new()
  assert_eq(empty_carrier.keys().length(), 0)
  
  // 测试包含特殊字符的header值
  let special_carrier = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key1=value1,key2=value%20with%20spaces,key3=value3!@#$%"),
    ("custom-header", "custom-value-with-special-chars:;!@#$%^&*()")
  ])
  
  // 验证traceparent格式
  match special_carrier.get("traceparent") {
    Some(trace_parent) => {
      // 验证格式: version-trace_id-span_id-trace_flags
      let parts = trace_parent.split("-")
      assert_eq(parts.length(), 4)
      assert_eq(parts[0], "00")  // version
      assert_eq(parts[1].length(), 32)  // trace_id (16 bytes = 32 hex chars)
      assert_eq(parts[2].length(), 16)  // span_id (8 bytes = 16 hex chars)
      assert_eq(parts[3].length(), 2)   // trace_flags (1 byte = 2 hex chars)
    }
    None => @test.fail("Expected traceparent header")
  }
  
  // 验证包含特殊字符的header
  match special_carrier.get("custom-header") {
    Some(value) => {
      assert_eq(value.contains(":;!@#$%^&*()"), true)
    }
    None => @test.fail("Expected custom-header")
  }
}

test "carrier_manipulation_operations" {
  // 测试Carrier的复杂操作
  
  let carrier = MapCarrier::new()
  
  // 测试设置和获取
  carrier.set("key1", "value1")
  carrier.set("key2", "value2")
  carrier.set("key3", "value3")
  
  // 验证所有键都存在
  let keys = carrier.keys()
  assert_eq(keys.length(), 3)
  assert_eq(keys.contains("key1"), true)
  assert_eq(keys.contains("key2"), true)
  assert_eq(keys.contains("key3"), true)
  
  // 测试覆盖值
  carrier.set("key1", "new_value1")
  match carrier.get("key1") {
    Some(value) => assert_eq(value, "new_value1")
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试删除操作（如果支持）
  // 注意：这里假设MapCarrier有delete方法，如果没有，这个测试需要调整
  
  // 测试大小写敏感性
  carrier.set("CaseSensitive", "value")
  match carrier.get("casesensitive") {
    Some(_) => @test.fail("Should be case sensitive")
    None => assert_eq(true, true)
  }
  
  match carrier.get("CaseSensitive") {
    Some(value) => assert_eq(value, "value")
    None => @test.fail("Expected Some(value)")
  }
}