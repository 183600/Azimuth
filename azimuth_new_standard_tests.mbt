// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(-2, add(5, -7))
  assert_eq(24, multiply(6, 4))
  assert_eq(-18, multiply(-3, 6))
  assert_eq(0, multiply(0, 12345))
}

test "boundary_value_tests" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法测试
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
}

test "string_functionality_tests" {
  // 测试字符串功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 5)), add(multiply(a, b), multiply(a, 5)))
  
  // 结合律
  assert_eq(add(add(a, b), 5), add(a, add(b, 5)))
}

test "real_world_calculations" {
  // 测试实际应用场景计算
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let cart_total = add(multiply(item_price, quantity), shipping)
  assert_eq(612, cart_total)
  
  // 温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 溢出保护验证
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算矩形周长：2 × (长 + 宽)
  let length = 12
  let width = 8
  let perimeter = multiply(2, add(length, width))
  assert_eq(40, perimeter)
  
  // 计算复利（简化版）
  let principal = 1000
  let rate = 5
  let years = 3
  let total = multiply(principal, add(1, multiply(rate, years)))
  assert_eq(16000, total)
}

test "performance_patterns" {
  // 测试性能模式
  // 级数求和：1 + 2 + 3 + 4 + 5
  let sum_1_to_5 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum_1_to_5)
  
  // 平方和：1² + 2² + 3² + 4² + 5²
  let sum_squares = add(add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4)), multiply(5, 5))
  assert_eq(55, sum_squares)
}

test "data_type_operations" {
  // 测试数据类型操作
  // 字符串拼接测试
  let name = "Alice"
  let age = "25"
  let result = greet(name + " (" + age + ")")
  assert_eq_string("Hello, Alice (25)!", result)
  
  // 数值范围测试
  assert_true(add(1000000, 2000000) > 0)
  assert_true(multiply(-1000, 1000) < 0)
  assert_eq(0, multiply(0, 123456789))
}

test "business_logic_scenarios" {
  // 测试业务逻辑场景
  // 订单计算
  let unit_price = 99
  let quantity = 5
  let discount = 50
  let tax_rate = 8
  
  let subtotal = multiply(unit_price, quantity)
  let after_discount = add(subtotal, -discount)
  let tax = multiply(after_discount, tax_rate) / 100
  let total = add(after_discount, tax)
  
  assert_eq(531, total)
  
  // 库存管理
  let initial_stock = 100
  let sold = 30
  let restocked = 20
  let current_stock = add(add(initial_stock, -sold), restocked)
  assert_eq(90, current_stock)
}