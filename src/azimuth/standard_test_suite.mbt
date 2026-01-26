// 标准 MoonBit 测试套件
// 为 azimuth 库添加的高质量测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(7, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-6, multiply(-2, 3))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "combined_operations" {
  // 组合运算测试
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
  
  let result3 = divide_with_ceil(add(10, 5), 4)
  assert_eq(4, result3)  // ceil(15/4) = 4
}

test "practical_business_scenarios" {
  // 实际业务场景测试
  // 订单总价计算
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(200, subtotal)
  assert_eq(215, total)
  
  // 包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 单位运算
  assert_eq(7, multiply(7, 1))
  assert_eq(7, add(7, 0))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // �数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
}

test "complex_calculations" {
  // 复杂计算测试
  // 多层嵌套计算
  let complex_result = add(
    multiply(10, divide_with_ceil(25, 4)),
    multiply(5, divide_with_ceil(17, 3))
  )
  assert_eq(85, complex_result)  // 10*7 + 5*6 = 70 + 15 = 85
  
  // 数学级数计算
  let sum_formula = divide_with_ceil(
    multiply(10, add(multiply(2, 3), multiply(9, 2))),
    2
  )
  assert_eq(120, sum_formula)  // ceil(10*(6+18)/2) = ceil(120) = 120
}

test "string_processing_special_cases" {
  // 字符串处理特殊情况测试
  // 空字符串和空白字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello,   !", greet("  "))
  
  // 特殊字符和符号
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
  
  // Unicode 字符
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}