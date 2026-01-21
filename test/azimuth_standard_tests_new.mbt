// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库函数的综合测试

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(100, 0))
  assert_eq(2147483647, add(2147483646, 1))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_function_comprehensive" {
  // 全面测试向上取整除法函数
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-11, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "complex_mathematical_operations" {
  // 复杂数学运算测试
  let result1 = add(multiply(3, 4), multiply(5, 6))
  assert_eq(42, result1)  // 3*4 + 5*6 = 12 + 30 = 42
  
  let result2 = divide_with_ceil(add(15, 10), 5)
  assert_eq(5, result2)  // ceil(25/5) = 5
  
  let result3 = multiply(divide_with_ceil(20, 3), add(2, 3))
  assert_eq(35, result3)  // ceil(20/3) * (2+3) = 7 * 5 = 35
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  // 计算订单总价：商品价格 × 数量 + 运费
  let product_price = 25
  let quantity = 4
  let shipping = 10
  
  let subtotal = multiply(product_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(110, total)  // 25*4 + 10 = 100 + 10 = 110
}

test "packaging_calculation_scenario" {
  // 包装计算场景测试
  // 计算需要的包装箱数量
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "time_management_scenario" {
  // 时间管理场景测试
  // 计算完成任务需要的天数
  let tasks_per_day = 6
  let total_tasks = 25
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  let total_hours = multiply(days_needed, 8)  // 每天8小时
  
  assert_eq(5, days_needed)  // ceil(25/6) = 5
  assert_eq(40, total_hours)  // 5 * 8 = 40
}