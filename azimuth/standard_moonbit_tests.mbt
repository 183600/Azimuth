// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(10, 8))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let a = 7
  let b = 13
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(7, 13))
  assert_eq(91, multiply(7, 13))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, space !", greet("space "))
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 42
  
  // 加法单位元: x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元: x * 1 = x
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元: x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 减法恒等式: x - x = 0
  assert_eq(0, subtract(x, x))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 库存管理场景
  let total_products = 157
  let box_capacity = 24
  let cost_per_product = 15
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(7, boxes_needed)
  
  // 计算总成本
  let total_cost = multiply(total_products, cost_per_product)
  assert_eq(2355, total_cost)
  
  // 计算每箱平均成本（向上取整）
  let cost_per_box = divide_with_ceil(total_cost, boxes_needed)
  assert_eq(337, cost_per_box)
}

test "workflow_calculation_sequence" {
  // 工作流计算序列测试
  // 项目资源规划场景
  let team_members = 8
  let tasks_per_member = 6
  let hours_per_task = 4
  let working_days = 5
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(48, total_tasks)
  
  // 计算总工时
  let total_hours = multiply(total_tasks, hours_per_task)
  assert_eq(192, total_hours)
  
  // 计算需要的工时数（向上取整）
  let weeks_needed = divide_with_ceil(total_hours, multiply(working_days, 8))
  assert_eq(5, weeks_needed)
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 极小值测试
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 极大值测试
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(10001, divide_with_ceil(10000, 1))
  
  // 特殊除法情况
  assert_eq(1, divide_with_ceil(9999, 10000))
  assert_eq(10000, divide_with_ceil(10000, 1))
  assert_eq(1, divide_with_ceil(10000, 10000))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  // O(1) 常数时间操作
  let constant_result = add(5, 3)
  assert_eq(8, constant_result)
  
  // O(n) 线性时间操作模拟 - 计算1到n的和
  let n = 10
  let linear_sum = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, linear_sum)
  
  // O(log n) 对数时间操作模拟 - 二分查找步骤
  let array_size = 1000
  let search_steps = divide_with_ceil(divide_with_ceil(array_size, 2), 2)
  assert_eq(250, search_steps)
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试
  // 购物车和订单计算
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  let tax_rate = 10
  
  // 计算各商品小计
  let item1_total = multiply(item1_price, item1_quantity)
  let item2_total = multiply(item2_price, item2_quantity)
  
  // 计算商品总计
  let subtotal = add(item1_total, item2_total)
  assert_eq(105, subtotal)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  assert_eq(11, tax_amount)
  
  // 计算订单总额
  let grand_total = add(subtotal, tax_amount)
  assert_eq(116, grand_total)
  
  // 计算需要的包装箱数（每箱最多装10件商品）
  let total_items = add(item1_quantity, item2_quantity)
  let packages_needed = divide_with_ceil(total_items, 10)
  assert_eq(1, packages_needed)
}