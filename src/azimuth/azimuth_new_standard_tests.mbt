// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的高质量测试

test "azimuth_core_addition_comprehensive" {
  // 全面的加法功能测试
  assert_eq(10, add(3, 7))  // 基本加法
  assert_eq(0, add(0, 0))   // 零值加法
  assert_eq(-5, add(-2, -3))  // 负数加法
  assert_eq(5, add(-2, 7))   // 正负数混合
  assert_eq(1000000, add(500000, 500000))  // 大数加法
}

test "azimuth_core_multiplication_comprehensive" {
  // 全面的乘法功能测试
  assert_eq(15, multiply(3, 5))  // 基本乘法
  assert_eq(0, multiply(0, 10))  // 零乘法
  assert_eq(-12, multiply(-3, 4))  // 负数乘法
  assert_eq(12, multiply(-3, -4))  // 负负得正
  assert_eq(10000, multiply(100, 100))  // 大数乘法
}

test "azimuth_core_divide_with_ceil_edge_cases" {
  // 向上取整除法的边界情况测试
  assert_eq(4, divide_with_ceil(10, 3))   // 10/3 = 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 整除情况
  assert_eq(1, divide_with_ceil(1, 100))  // 小数除大数
  assert_eq(-3, divide_with_ceil(-10, 3)) // 负数除法
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为零的安全处理
}

test "azimuth_core_greet_functionality" {
  // 问候功能的全面测试
  assert_eq_string("Hello, World!", greet("World"))  // 基本问候
  assert_eq_string("Hello, !", greet(""))            // 空字符串
  assert_eq_string("Hello, 中文!", greet("中文"))    // 中文字符
  assert_eq_string("Hello, 🚀!", greet("🚀"))        // Emoji字符
  assert_eq_string("Hello, 123!", greet("123"))      // 数字字符串
}

test "azimuth_core_mathematical_properties" {
  // 数学性质的验证测试
  let a = 8
  let b = 13
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a × b = b × a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质: a + 0 = a, a × 0 = 0
  assert_eq(a, add(a, 0))
  assert_eq(0, multiply(a, 0))
}

test "azimuth_core_business_scenario_calculations" {
  // 业务场景计算测试
  // 订单总价计算
  let unit_price = 45
  let quantity = 6
  let shipping = 15
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(270, subtotal)
  assert_eq(285, total)
  
  // 包装需求计算
  let total_items = 37
  let items_per_box = 8
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(37/8) = 5
}

test "azimuth_core_complex_expressions" {
  // 复杂表达式计算测试
  let x = 10
  let y = 15
  let z = 20
  
  // 测试 (x + y) × z
  let result1 = multiply(add(x, y), z)
  assert_eq(500, result1)  // (10 + 15) × 20 = 25 × 20 = 500
  
  // 测试 x × y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(170, result2)  // 10 × 15 + 20 = 150 + 20 = 170
  
  // 测试嵌套表达式
  let result3 = divide_with_ceil(multiply(add(x, y), z), 3)
  assert_eq(167, result3)  // ceil(500/3) = 167
}

test "azimuth_core_boundary_value_testing" {
  // 边界值测试
  // 测试极值运算
  assert_eq(1, add(0, 1))        // 最小正整数
  assert_eq(-1, add(0, -1))      // 最大负整数
  assert_eq(1, divide_with_ceil(1, 1))  // 单位除法
  assert_eq(1, multiply(1, 1))   // 单位乘法
  
  // 测试大数精度
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(1, divide_with_ceil(large_num, large_num))
}

test "azimuth_core_real_world_applications" {
  // 真实世界应用测试
  // 库存管理
  let initial_stock = 120
  let daily_sales = 8
  let days = 7
  let final_stock = add(initial_stock, -multiply(daily_sales, days))
  assert_eq(64, final_stock)  // 120 - (8 × 7) = 64
  
  // 项目时间规划
  let total_tasks = 45
  let tasks_per_day = 6
  let work_days = divide_with_ceil(total_tasks, tasks_per_day)
  assert_eq(8, work_days)  // ceil(45/6) = 8
  
  // 财务计算
  let revenue = 5000
  let expenses = 3200
  let profit = add(revenue, -expenses)
  assert_eq(1800, profit)
}

test "azimuth_core_error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零安全性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 字符串边界情况
  assert_eq_string("Hello,   !", greet("   "))  // 空格字符串
  assert_eq_string("Hello, \t!", greet("\t"))  // 制表符
  assert_eq_string("Hello, \n!", greet("\n"))  // 换行符
  
  // 数值边界情况
  assert_eq(0, multiply(0, 0))
  assert_eq(0, add(0, 0))
}