// 新增的高质量 MoonBit 测试用例
// 专注于函数组合和复杂计算场景

test "function_composition_basic" {
  // 测试基本函数组合
  let initial = 5
  let result = add(multiply(initial, 2), 3)  // 5*2 + 3 = 13
  assert_eq(13, result)
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let base = 10
  let step1 = multiply(base, 3)        // 30
  let step2 = add(step1, 5)            // 35
  let step3 = divide_with_ceil(step2, 8)  // ceil(35/8) = 5
  assert_eq(5, step3)
}

test "mathematical_properties_extended" {
  // 测试扩展的数学性质
  let x = 15
  let y = 7
  
  // 分配律验证
  let distributive_left = multiply(x, add(y, 2))
  let distributive_right = add(multiply(x, y), multiply(x, 2))
  assert_eq(distributive_left, distributive_right)
}

test "business_logic_simulation" {
  // 业务逻辑模拟测试
  let items = 47
  let capacity = 12
  let containers = divide_with_ceil(items, capacity)  // ceil(47/12) = 4
  let total_capacity = multiply(containers, capacity)  // 4*12 = 48
  let unused_space = subtract(total_capacity, items)  // 48-47 = 1
  
  assert_eq(4, containers)
  assert_eq(48, total_capacity)
  assert_eq(1, unused_space)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "performance_calculation" {
  // 性能计算测试
  let large_number = 10000
  let operations = 100
  let result = divide_with_ceil(multiply(large_number, operations), 50000)
  assert_eq(20, result)
}

test "nested_function_calls" {
  // 嵌套函数调用测试
  let a = 8
  let b = 3
  let c = 2
  
  // 复杂嵌套：((a*b) + c) / (b+c)
  let numerator = add(multiply(a, b), c)  // 8*3 + 2 = 26
  let denominator = add(b, c)             // 3 + 2 = 5
  let result = divide_with_ceil(numerator, denominator)  // ceil(26/5) = 6
  
  assert_eq(6, result)
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  a - b
}