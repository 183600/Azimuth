// Azimuth 库综合测试套件
// 包含10个高质量的标准 MoonBit 测试用例

test "add_function_comprehensive" {
  // 全面测试加法函数
  // 基本功能测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  
  // 边界值测试
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-1000000, add(-500000, -500000))
  assert_eq(1, add(2147483647, -2147483646))
  
  // 数学性质验证
  let x = 42
  assert_eq(x, add(x, 0))  // 加法单位元
  assert_eq(add(x, 5), add(5, x))  // 交换律
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  // 基本功能测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 123))
  assert_eq(-12, multiply(-3, 4))
  
  // 边界值测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, multiply(-1, -1))
  assert_eq(-1, multiply(1, -1))
  
  // 数学性质验证
  let x = 17
  assert_eq(x, multiply(x, 1))  // 乘法单位元
  assert_eq(0, multiply(x, 0))  // 零元素性质
  assert_eq(multiply(x, 7), multiply(7, x))  // 交换律
}

test "divide_with_ceil_function_comprehensive" {
  // 全面测试向上取整除法函数
  // 正数除法测试
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 整除
  assert_eq(1, divide_with_ceil(1, 100)) // 小数
  
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))   // -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))   // 3.333... -> 4
  assert_eq(-4, divide_with_ceil(-11, 3))   // -3.666... -> -4
  
  // 边界情况测试
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为零
  assert_eq(0, divide_with_ceil(0, 5))    // 被除数为零
  assert_eq(1, divide_with_ceil(100, 100)) // 相等数
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  // 基本功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 特殊字符测试
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, 123!", greet("123"))  // 数字
  assert_eq_string("Hello, 世界!", greet("世界"))  // 中文
  assert_eq_string("Hello, 🚀!", greet("🚀"))  // Emoji
  
  // 复杂字符串测试
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "mathematical_distributive_law" {
  // 验证数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 8
  let c = 12
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 6 * (8 + 12) = 6 * 8 + 6 * 12 = 120
}

test "real_world_shopping_cart_calculation" {
  // 真实世界购物车计算场景
  let apple_price = 5
  let apple_qty = 6
  let banana_price = 3
  let banana_qty = 8
  let shipping_fee = 12
  
  // 计算商品总价
  let apple_total = multiply(apple_price, apple_qty)
  let banana_total = multiply(banana_price, banana_qty)
  let subtotal = add(apple_total, banana_total)
  
  // 计算最终总价
  let total = add(subtotal, shipping_fee)
  
  assert_eq(30, apple_total)  // 5 * 6
  assert_eq(24, banana_total)  // 3 * 8
  assert_eq(54, subtotal)     // 30 + 24
  assert_eq(66, total)        // 54 + 12
}

test "inventory_packaging_optimization" {
  // 库存包装优化场景
  let total_products = 157
  let box_capacity = 24
  let warehouse_sections = 3
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 计算每个仓库区域的箱子数量
  let boxes_per_section = divide_with_ceil(boxes_needed, warehouse_sections)
  
  assert_eq(7, boxes_needed)      // ceil(157/24) = 7
  assert_eq(3, boxes_per_section) // ceil(7/3) = 3
}

test "project_resource_allocation" {
  // 项目资源分配场景
  let team_members = 8
  let tasks_per_member = 5
  let overtime_tasks = 7
  let bonus_tasks = 3
  
  // 计算基础任务量
  let base_tasks = multiply(team_members, tasks_per_member)
  
  // 计算总任务量
  let total_tasks = add(add(base_tasks, overtime_tasks), bonus_tasks)
  
  // 计算需要的额外人手（假设每人最多处理6个任务）
  let additional_staff = divide_with_ceil(total_tasks, 6) - team_members
  
  assert_eq(40, base_tasks)        // 8 * 5
  assert_eq(50, total_tasks)       // 40 + 7 + 3
  assert_eq(1, additional_staff)   // ceil(50/6) - 8 = 9 - 8 = 1
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let interest_rate = 8  // 8%
  let years = 3
  let monthly_fee = 25
  
  // 计算年利息
  let annual_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  
  // 计算总利息（简化为单利）
  let total_interest = multiply(annual_interest, years)
  
  // 计算总费用
  let total_fees = multiply(monthly_fee, multiply(years, 12))
  
  // 计算最终金额
  let final_amount = add(add(principal, total_interest), total_fees)
  
  assert_eq(800, annual_interest)   // ceil(10000 * 8 / 100)
  assert_eq(2400, total_interest)   // 800 * 3
  assert_eq(900, total_fees)        // 25 * 3 * 12
  assert_eq(13300, final_amount)    // 10000 + 2400 + 900
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 测试除零安全性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(4294967294, add(2147483647, 2147483647))
  
  // 测试字符串边界情况
  assert_eq_string("Hello,   !", greet("   "))  // 空格字符串
  assert_eq_string("Hello, A!", greet("A"))     // 单字符
  
  // 测试数学运算的一致性
  let x = 100
  let y = 25
  assert_eq(multiply(x, divide_with_ceil(y, 5)), multiply(x, 5))  // 100 * ceil(25/5) = 100 * 5
}