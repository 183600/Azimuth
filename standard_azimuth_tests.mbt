// 标准 Azimuth 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(add(3, 5), 8)
  assert_eq(multiply(4, 7), 28)
  assert_eq(subtract(15, 9), 6)
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(1, 100), 100)
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(add(-3, 7), 4)
  assert_eq(add(5, -8), -3)
  assert_eq(multiply(-4, 3), -12)
  assert_eq(multiply(-2, -5), 10)
  assert_eq(subtract(-10, 5), -15)
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("用户"), "Hello, 用户!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  let a = 12
  let b = 8
  let c = 5
  
  // 复合运算：(a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 100)
  
  // 复合运算：a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 101)
  
  // 复合运算：(a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(result3, 1)
}

test "boundary_value_testing" {
  // 边界值测试
  // 测试零值
  assert_eq(add(0, 5), 5)
  assert_eq(multiply(0, 100), 0)
  assert_eq(subtract(10, 0), 10)
  assert_eq(divide_with_ceil(0, 10), 0)
  
  // 测试单位值
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 50), 50)
  assert_eq(subtract(1, 1), 0)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 测试负单位值
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 10), -10)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 15
  let y = 7
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
  
  // 减法的逆运算验证
  assert_eq(add(subtract(x, y), y), x)
}

test "real_world_application_simulation" {
  // 真实世界应用模拟测试
  // 模拟购物车计算
  let item_price = 25
  let quantity = 4
  let tax_rate = 10
  
  // 计算小计
  let subtotal = multiply(item_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(subtotal, tax)
  
  // 计算每箱包装数量（假设每箱最多装6个）
  let boxes_needed = divide_with_ceil(quantity, 6)
  
  assert_eq(subtotal, 100)
  assert_eq(tax, 10)
  assert_eq(total, 110)
  assert_eq(boxes_needed, 1)
}