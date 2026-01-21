// 质量导向的 MoonBit 测试用例
// 覆盖核心功能的边界情况和实际应用场景

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(8, add(3, 5))           // 基本正数加法
  assert_eq(-2, add(-5, 3))          // 正负数混合
  assert_eq(-8, add(-3, -5))         // 负数加法
  assert_eq(1000, add(1000, 0))      // 零元素性质
  assert_eq(0, add(2147483647, -2147483647))  // 大数边界
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(25, multiply(5, 5))      // 基本乘法
  assert_eq(-15, multiply(3, -5))    // 正负数乘法
  assert_eq(0, multiply(0, 12345))   // 零乘法
  assert_eq(1, multiply(-1, -1))     // 负负得正
  assert_eq(1000000, multiply(1000, 1000))  // 大数乘法
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(11, 3))   // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 完整除法
  assert_eq(-3, divide_with_ceil(-10, 3)) // 负数除法
  assert_eq(4, divide_with_ceil(-11, -3)) // 负数除负数
  assert_eq(0, divide_with_ceil(100, 0))  // 除数为零
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, !", greet(""))           // 空字符串
  assert_eq_string("Hello, 123!", greet("123"))     // 数字字符串
  assert_eq_string("Hello, 世界!", greet("世界"))   // 中文字符
  assert_eq_string("Hello, 🚀!", greet("🚀"))       // Emoji字符
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 12
  let y = 8
  
  // 交换律验证
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
}

test "real_world_shopping_cart" {
  // 真实购物车计算场景
  let apple_price = 5
  let apple_qty = 3
  let banana_price = 3
  let banana_qty = 4
  
  let apple_total = multiply(apple_price, apple_qty)
  let banana_total = multiply(banana_price, banana_qty)
  let cart_total = add(apple_total, banana_total)
  
  assert_eq(27, cart_total)  // 5*3 + 3*4 = 15 + 12 = 27
}

test "packaging_optimization" {
  // 包装优化计算
  let total_products = 47
  let box_capacity = 12
  let needed_boxes = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(4, needed_boxes)  // ceil(47/12) = 4
}

test "time_management_calculation" {
  // 时间管理计算
  let daily_tasks = 8
  let total_tasks = 35
  let required_days = divide_with_ceil(total_tasks, daily_tasks)
  let total_work_hours = multiply(required_days, 8)
  
  assert_eq(5, required_days)     // ceil(35/8) = 5
  assert_eq(40, total_work_hours) // 5 * 8 = 40
}

test "financial_budget_planning" {
  // 财务预算规划
  let monthly_income = 5000
  let rent = 1500
  let food = multiply(30, 50)  // 30天 * 50元/天
  let utilities = 200
  let savings_target = 1000
  
  let total_expenses = add(add(add(rent, food), utilities), savings_target)
  let remaining = add(monthly_income, -total_expenses)
  
  assert_eq(800, remaining)  // 5000 - (1500 + 1500 + 200 + 1000) = 800
}

test "complex_nested_operations" {
  // 复杂嵌套操作
  let base = 10
  let multiplier = 3
  let divisor = 4
  let offset = 5
  
  // 计算: (base * multiplier) / divisor + offset
  let step1 = multiply(base, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let final_result = add(step2, offset)
  
  assert_eq(13, final_result)  // ceil(10*3/4) + 5 = ceil(30/4) + 5 = 8 + 5 = 13
}