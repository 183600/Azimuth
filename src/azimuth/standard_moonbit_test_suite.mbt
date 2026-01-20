// 标准 MoonBit 测试套件
// 包含8个核心测试用例，覆盖基本功能

test "string_concatenation_basic" {
  // 测试基本字符串拼接功能
  let result = greet("MoonBit")
  assert_eq_string("Hello, MoonBit!", result)
}

test "arithmetic_operations_comprehensive" {
  // 测试综合算术运算
  let a = 15
  let b = 25
  
  // 加法测试
  let sum = add(a, b)
  assert_eq(40, sum)
  
  // 乘法测试
  let product = multiply(a, b)
  assert_eq(375, product)
  
  // 复合运算：(a + b) * a - b
  let complex = add(multiply(sum, a), -b)
  assert_eq(585, complex)
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, multiply(max_val, 2))
  
  // 最小值溢出保护
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 零值运算
  assert_eq(0, multiply(0, max_val))
  assert_eq(max_val, add(max_val, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  let z = 5
  
  // 交换律
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 结合律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let discount = 50
  let tax_rate = 8
  
  let subtotal = multiply(item_price, quantity)
  let after_discount = add(subtotal, -discount)
  let tax = multiply(after_discount, tax_rate) / 100
  let total = add(after_discount, tax)
  
  assert_eq(608, total)
  
  // 温度转换（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  // 空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  
  // 长字符串
  let long_name = "VeryLongUserNameForTestingPurposes"
  assert_eq_string("Hello, VeryLongUserNameForTestingPurposes!", greet(long_name))
  
  // Unicode字符
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "error_handling_validation" {
  // 测试错误处理和验证
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 边界条件验证
  let normal_add = add(100, 200)
  assert_eq(300, normal_add)
  
  let normal_mul = multiply(15, 4)
  assert_eq(60, normal_mul)
  
  // 验证溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}

test "performance_efficient_calculations" {
  // 测试性能相关的计算
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  assert_eq(3000000, add(large_num1, large_num2))
  assert_eq(2000000000000, multiply(large_num1, large_num2))
  
  // 序列计算：1+2+3+...+10 = 55
  let series_sum = add(add(add(add(add(1, 2), add(3, 4)), add(5, 6)), add(add(7, 8), add(9, 10)))
  assert_eq(55, series_sum)
  
  // 幂运算：2^6 = 64
  let power_of_2 = multiply(multiply(multiply(2, 2), multiply(2, 2)), multiply(2, 2))
  assert_eq(64, power_of_2)
}