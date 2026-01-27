// 综合 MoonBit 标准测试用例
// 为 azimuth 库提供全面的核心功能测试，包含10个测试用例

test "add_function_comprehensive" {
  // 全面测试加法功能
  // 基本加法
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  // 负数加法
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  // 零元素加法
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  // 大数加法
  @azimuth.assert_eq(10000, @azimuth.add(5000, 5000))
  // 交换律验证
  @azimuth.assert_eq(@azimuth.add(12, 8), @azimuth.add(8, 12))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  // 基本乘法
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  // 负数乘法
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  // 零元素乘法
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  // 单位元乘法
  @azimuth.assert_eq(9, @azimuth.multiply(9, 1))
  // 交换律验证
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  // 基本问候
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  // 空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  // 数字字符串
  @azimuth.assert_eq_string("Hello, 2024!", @azimuth.greet("2024"))
  // Unicode 字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  // 特殊字符
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  // 除零安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  // 小数向上取整
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 15
  let b = 7
  let c = 3
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价计算
  let unit_price = 45
  let quantity = 6
  let discount = 20
  let shipping = 15
  let tax_rate = 8
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算折扣前总价
  let pre_discount_total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  // 应用折扣
  let final_total = @azimuth.add(pre_discount_total, -discount)
  
  @azimuth.assert_eq(270, subtotal)
  @azimuth.assert_eq(22, tax)
  @azimuth.assert_eq(307, pre_discount_total)
  @azimuth.assert_eq(287, final_total)
}

test "resource_allocation_scenario" {
  // 资源分配场景：计算项目所需资源
  let total_tasks = 47
  let tasks_per_person = 5
  let work_days = 6
  
  // 计算所需人员
  let people_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_person)
  // 计算总工作日
  let total_work_days = @azimuth.multiply(people_needed, work_days)
  // 计算每人平均任务数
  let avg_tasks_per_person = @azimuth.divide_with_ceil(total_tasks, people_needed)
  
  @azimuth.assert_eq(10, people_needed)
  @azimuth.assert_eq(60, total_work_days)
  @azimuth.assert_eq(5, avg_tasks_per_person)
}

test "edge_cases_and_boundary_values" {
  // 边界值和极端情况测试
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  
  // 单位运算
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(99, 100))
  
  // 极端除法
  @azimuth.assert_eq(100000, @azimuth.divide_with_ceil(100000, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100000))
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  let base_value = 10
  let multiplier = 3
  let offset = 5
  let divisor = 4
  
  // 嵌套计算：(base * multiplier + offset) / divisor
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.add(step1, offset)
  let final_result = @azimuth.divide_with_ceil(step2, divisor)
  
  @azimuth.assert_eq(30, step1)
  @azimuth.assert_eq(35, step2)
  @azimuth.assert_eq(9, final_result)
  
  // 更复杂的嵌套
  let complex_result = @azimuth.add(
    @azimuth.multiply(@azimuth.divide_with_ceil(25, 4), 3),
    @azimuth.multiply(@azimuth.divide_with_ceil(17, 3), 2)
  )
  @azimuth.assert_eq(31, complex_result)
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合测试
  let first_name = "张"
  let last_name = "三"
  let title = "工程师"
  
  // 构建完整姓名
  let full_name = first_name + last_name
  // 构建带职位的称呼
  let full_title = full_name + title
  // 生成问候
  let greeting = @azimuth.greet(full_title)
  
  @azimuth.assert_eq_string("张三", full_name)
  @azimuth.assert_eq_string("张三工程师", full_title)
  @azimuth.assert_eq_string("Hello, 张三工程师!", greeting)
  
  // 测试特殊字符串组合
  let special_chars = "@azimuth"
  let special_greeting = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, @azimuth!", special_greeting)
}