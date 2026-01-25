// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能测试
// 包含 10 个高质量测试用例，覆盖所有主要功能

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(5, add(2, 3))           // 基本正数加法
  assert_eq(0, add(0, 0))           // 零加法
  assert_eq(-5, add(-2, -3))        // 负数加法
  assert_eq(3, add(-2, 5))          // 正负数混合
  assert_eq(1000, add(500, 500))    // 大数加法
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(6, multiply(2, 3))      // 基本正数乘法
  assert_eq(0, multiply(0, 5))      // 零乘法
  assert_eq(-6, multiply(-2, 3))    // 负数乘法
  assert_eq(15, multiply(-3, -5))   // 负负得正
  assert_eq(10000, multiply(100, 100)) // 大数乘法
}

test "divide_with_ceil_various_cases" {
  // 测试向上取整除法的各种情况
  assert_eq(4, divide_with_ceil(10, 3))   // 正数向上取整
  assert_eq(3, divide_with_ceil(9, 3))    // 整除情况
  assert_eq(-3, divide_with_ceil(-10, 3)) // 负数向上取整
  assert_eq(4, divide_with_ceil(-10, -3)) // 负数除负数
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为零的安全处理
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串处理
  assert_eq_string("Hello, María!", greet("María")) // 特殊字符
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_business_calculations" {
  // 复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "packaging_and_logistics_scenario" {
  // 包装和物流场景计算
  let total_items = 47
  let items_per_box = 12
  let boxes_per_pallet = 4
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let pallets_needed = divide_with_ceil(boxes_needed, boxes_per_pallet)
  
  assert_eq(4, boxes_needed)   // ceil(47/12) = 4
  assert_eq(1, pallets_needed) // ceil(4/4) = 1
}

test "financial_calculation_scenario" {
  // 金融计算场景
  let principal = 1000
  let interest_rate = 5
  let years = 3
  
  // 简化的复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 除数为零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极端数值
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}

test "combined_arithmetic_expressions" {
  // 组合算术表达式
  let x = 10
  let y = 15
  let z = 20
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(500, result1)  // (10 + 15) * 20 = 25 * 20 = 500
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(170, result2)  // 10 * 15 + 20 = 150 + 20 = 170
  
  // 测试复杂表达式
  let complex = add(multiply(divide_with_ceil(add(x, y), 5), z), 100)
  assert_eq(200, complex)  // ceil(25/5) * 20 + 100 = 5 * 20 + 100 = 200
}