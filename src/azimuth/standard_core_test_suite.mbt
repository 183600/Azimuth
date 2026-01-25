// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含 10 个高质量测试用例，覆盖所有主要功能

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(5, add(2, 3))  // 基本正数加法
  assert_eq(0, add(0, 0))  // 零值加法
  assert_eq(-5, add(-2, -3))  // 负数加法
  assert_eq(10, add(15, -5))  // 正负数混合
  assert_eq(1000000, add(500000, 500000))  // 大数加法
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(6, multiply(2, 3))  // 基本正数乘法
  assert_eq(0, multiply(0, 5))  // 零乘法
  assert_eq(-6, multiply(-2, 3))  // 负数乘法
  assert_eq(15, multiply(-3, -5))  // 负负得正
  assert_eq(10000, multiply(100, 100))  // 大数乘法
}

test "divide_with_ceil_function_comprehensive" {
  // 全面测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))  // 需要向上取整
  assert_eq(3, divide_with_ceil(9, 3))   // 整除
  assert_eq(1, divide_with_ceil(1, 100)) // 被除数小于除数
  assert_eq(-3, divide_with_ceil(-10, 3)) // 负数除法
  assert_eq(0, divide_with_ceil(5, 0))   // 除数为零的安全处理
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  assert_eq_string("Hello, World!", greet("World"))  // 基本功能
  assert_eq_string("Hello, !", greet(""))  // 空字符串处理
  assert_eq_string("Hello, 123!", greet("123"))  // 数字字符串
  assert_eq_string("Hello, 中文!", greet("中文"))  // 中文字符
  assert_eq_string("Hello, 🚀!", greet("🚀"))  // Emoji 字符
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
  
  // 分配律
  assert_eq(multiply(a, add(b, 2)), add(multiply(a, b), multiply(a, 2)))
}

test "complex_business_scenarios" {
  // 复杂业务场景测试
  // 计算订单总价：商品价格 × 数量 + 运费
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "packaging_and_distribution_calculation" {
  // 包装和分销计算
  let total_items = 47
  let items_per_box = 12
  
  // 计算需要的包装箱数量
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 计算总成本（假设每个箱子成本5元）
  let box_cost = 5
  let total_cost = multiply(boxes_needed, box_cost)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  assert_eq(20, total_cost)
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试特殊字符处理
  let special_name = "Alice & Bob"
  let special_greeting = greet(special_name)
  assert_eq_string("Hello, Alice & Bob!", special_greeting)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 测试除数为零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 浮点数边界情况（通过整数模拟）
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(100, divide_with_ceil(9999, 100))  // 99.99 -> 100
}

test "performance_and_large_scale_operations" {
  // 性能和大规模运算测试
  let large_number = 100000
  
  // 大数运算
  let large_sum = add(large_number, large_number)
  let large_product = multiply(large_number, 100)
  let large_division = divide_with_ceil(large_sum, 3)
  
  assert_eq(200000, large_sum)
  assert_eq(10000000, large_product)
  assert_eq(66667, large_division)  // ceil(200000/3) = 66667
  
  // 复杂表达式计算
  let complex_result = add(
    multiply(10, divide_with_ceil(25, 4)),
    multiply(5, divide_with_ceil(17, 3))
  )
  assert_eq(85, complex_result)  // 10*7 + 5*6 = 70 + 15 = 85
}