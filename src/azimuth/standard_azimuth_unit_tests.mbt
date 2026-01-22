// 标准 Azimuth 单元测试
// 测试 azimuth 库的核心功能

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(-2, 7))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_basic" {
  // 测试问候函数基本功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
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

test "complex_calculations" {
  // 测试复杂计算场景
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = divide_with_ceil(add(multiply(3, 4), multiply(5, 2)), 7)
  assert_eq(4, result2)  // ceil((12 + 10)/7) = ceil(22/7) = 4
}

test "edge_cases_handling" {
  // 测试边界情况处理
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, multiply(5, 0))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(105, total)  // 25*3 + 15*2 = 75 + 30 = 105
  
  // 计算需要的包装箱数量
  let total_items = 23
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(23/5) = 5
}

test "string_processing_with_greet" {
  // 测试字符串处理与问候函数结合
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试特殊字符
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "comprehensive_business_calculation" {
  // 综合业务计算测试
  let product_price = 120
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(subtotal, tax)
  let final_total = add(add(total_before_discount, shipping), -discount)
  
  assert_eq(960, subtotal)
  assert_eq(77, tax)
  assert_eq(1037, total_before_discount)
  assert_eq(1002, final_total)
}