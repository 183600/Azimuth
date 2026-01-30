// 标准 MoonBit 测试用例 - 为 azimuth 库编写的高质量测试
// 使用标准的 MoonBit 测试语法，包含10个全面的测试用例

// 导入被测试的模块
use azimuth

test "add_function_comprehensive_test" {
  // 全面测试加法函数
  // 基本正数加法
  azimuth.assert_eq(8, azimuth.add(3, 5))
  // 包含零的加法
  azimuth.assert_eq(7, azimuth.add(7, 0))
  azimuth.assert_eq(7, azimuth.add(0, 7))
  // 负数加法
  azimuth.assert_eq(-8, azimuth.add(-3, -5))
  azimuth.assert_eq(2, azimuth.add(-3, 5))
  azimuth.assert_eq(-2, azimuth.add(3, -5))
  // 大数加法
  azimuth.assert_eq(2000, azimuth.add(1000, 1000))
}

test "multiply_function_comprehensive_test" {
  // 全面测试乘法函数
  // 基本正数乘法
  azimuth.assert_eq(15, azimuth.multiply(3, 5))
  // 包含零的乘法
  azimuth.assert_eq(0, azimuth.multiply(7, 0))
  azimuth.assert_eq(0, azimuth.multiply(0, 7))
  // 负数乘法
  azimuth.assert_eq(15, azimuth.multiply(-3, -5))
  azimuth.assert_eq(-15, azimuth.multiply(-3, 5))
  azimuth.assert_eq(-15, azimuth.multiply(3, -5))
  // 单位元乘法
  azimuth.assert_eq(7, azimuth.multiply(7, 1))
  azimuth.assert_eq(7, azimuth.multiply(1, 7))
}

test "greet_function_comprehensive_test" {
  // 全面测试问候函数
  // 基本英文名字
  azimuth.assert_eq_string("Hello, Alice!", azimuth.greet("Alice"))
  // 空字符串处理
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  // 中文字符
  azimuth.assert_eq_string("Hello, 张三!", azimuth.greet("张三"))
  // 包含空格的名字
  azimuth.assert_eq_string("Hello, John Doe!", azimuth.greet("John Doe"))
  // 数字和特殊字符
  azimuth.assert_eq_string("Hello, User123!", azimuth.greet("User123"))
  azimuth.assert_eq_string("Hello, test@example.com!", azimuth.greet("test@example.com"))
}

test "divide_with_ceil_function_positive_test" {
  // 测试正数的向上取整除法
  // 能整除的情况
  azimuth.assert_eq(4, azimuth.divide_with_ceil(12, 3))
  azimuth.assert_eq(5, azimuth.divide_with_ceil(15, 3))
  // 不能整除需要向上取整
  azimuth.assert_eq(5, azimuth.divide_with_ceil(13, 3))
  azimuth.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  // 被除数小于除数
  azimuth.assert_eq(1, azimuth.divide_with_ceil(2, 5))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 100))
}

test "divide_with_ceil_function_negative_test" {
  // 测试负数的向上取整除法
  // 负被除数，正除数
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-8, 3))
  azimuth.assert_eq(-2, azimuth.divide_with_ceil(-6, 3))
  // 正被除数，负除数
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(8, -3))
  azimuth.assert_eq(-2, azimuth.divide_with_ceil(6, -3))
  // 负被除数，负除数
  azimuth.assert_eq(3, azimuth.divide_with_ceil(-8, -3))
  azimuth.assert_eq(2, azimuth.divide_with_ceil(-6, -3))
}

test "divide_with_ceil_function_edge_cases" {
  // 测试向上取整除法的边界情况
  // 除数为零的安全处理
  azimuth.assert_eq(0, azimuth.divide_with_ceil(10, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-10, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  // 零被除数
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 5))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, -5))
  // 相等的数值
  azimuth.assert_eq(1, azimuth.divide_with_ceil(5, 5))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(-5, -5))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 6
  let b = 9
  let c = 4
  
  // 加法交换律
  azimuth.assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  // 乘法交换律
  azimuth.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  // 加法结合律
  azimuth.assert_eq(azimuth.add(azimuth.add(a, b), c), azimuth.add(a, azimuth.add(b, c)))
  // 乘法结合律
  azimuth.assert_eq(azimuth.multiply(azimuth.multiply(a, b), c), azimuth.multiply(a, azimuth.multiply(b, c)))
  // 分配律
  azimuth.assert_eq(azimuth.multiply(a, azimuth.add(b, c)), azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c)))
}

test "real_world_business_calculation" {
  // 真实世界业务计算场景
  // 计算订单总价：商品价格 × 数量 + 运费
  let unit_price = 25
  let quantity = 8
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = azimuth.multiply(unit_price, quantity)
  let total_before_discount = azimuth.add(subtotal, shipping_cost)
  let final_total = azimuth.add(total_before_discount, -discount)
  
  azimuth.assert_eq(200, subtotal)
  azimuth.assert_eq(215, total_before_discount)
  azimuth.assert_eq(195, final_total)
}

test "packaging_and_shipping_calculation" {
  // 包装和运输计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = azimuth.divide_with_ceil(total_items, items_per_box)
  
  // 计算总容量
  let total_capacity = azimuth.multiply(boxes_needed, items_per_box)
  
  // 计算剩余空间
  let remaining_space = azimuth.add(total_capacity, -total_items)
  
  azimuth.assert_eq(4, boxes_needed)
  azimuth.assert_eq(48, total_capacity)
  azimuth.assert_eq(1, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算
  // 项目管理场景：计算项目完成时间和资源需求
  let team_size = 6
  let tasks_per_person_per_day = 3
  let total_tasks = 85
  let working_days_per_week = 5
  
  // 计算每日处理能力
  let daily_capacity = azimuth.multiply(team_size, tasks_per_person_per_day)
  
  // 计算需要的总工作日
  let total_workdays_needed = azimuth.divide_with_ceil(total_tasks, daily_capacity)
  
  // 计算需要的周数
  let weeks_needed = azimuth.divide_with_ceil(total_workdays_needed, working_days_per_week)
  
  azimuth.assert_eq(18, daily_capacity)
  azimuth.assert_eq(5, total_workdays_needed)
  azimuth.assert_eq(1, weeks_needed)
}