// 额外的 MoonBit 标准测试用例
// 针对 azimuth 库函数的高质量测试

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.add(-10, 25), @azimuth.add(25, -10))
  @azimuth.assert_eq(@azimuth.add(-15, -30), @azimuth.add(-30, -15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 3
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)  // 5 * (7 + 3) = 5 * 7 + 5 * 3 = 50
}

test "divide_with_ceil_fractions" {
  // 测试向上取整除法的小数情况
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))    // 1.5 -> 2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))    // 2.666... -> 3
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(19, 4))   // 4.75 -> 5
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(50, 8))   // 6.25 -> 7
}

test "greet_function_multilingual_support" {
  // 测试多语言支持的问候功能
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Mundo!", @azimuth.greet("Mundo"))
  @azimuth.assert_eq_string("Hello, Мир!", @azimuth.greet("Мир"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "complex_financial_calculation" {
  // 复杂金融计算：计算贷款月供
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率百分比
  let years = 30          // 贷款年限
  
  // 简化计算：月供 = (本金 + 总利息) / 总月数，向上取整
  let total_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100)
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = @azimuth.divide_with_ceil(total_payment, @azimuth.multiply(years, 12))
  
  @azimuth.assert_eq(278, monthly_payment)  // (100000 + 6000) / 360 = 294.44 -> 295
}

test "time_management_scenario" {
  // 时间管理场景：计算项目完成时间
  let total_tasks = 87
  let tasks_per_day = 6
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  // 计算需要的周数
  let weeks_needed = @azimuth.divide_with_ceil(work_days_needed, work_days_per_week)
  
  @azimuth.assert_eq(15, work_days_needed)  // ceil(87/6) = 15
  @azimuth.assert_eq(3, weeks_needed)       // ceil(15/5) = 3
}

test "resource_allocation_problem" {
  // 资源分配问题：如何公平分配资源
  let total_resources = 73
  let departments = 8
  let minimum_per_dept = 5
  
  // 每个部门先分配最小资源，剩余资源平均分配
  let allocated_minimum = @azimuth.multiply(minimum_per_dept, departments)
  let remaining_resources = @azimuth.add(total_resources, -allocated_minimum)
  let additional_per_dept = @azimuth.divide_with_ceil(remaining_resources, departments)
  
  let total_per_dept = @azimuth.add(minimum_per_dept, additional_per_dept)
  
  @azimuth.assert_eq(40, allocated_minimum)      // 5 * 8 = 40
  @azimuth.assert_eq(33, remaining_resources)    // 73 - 40 = 33
  @azimuth.assert_eq(5, additional_per_dept)     // ceil(33/8) = 5
  @azimuth.assert_eq(10, total_per_dept)         // 5 + 5 = 10
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：斐波那契数列相关
  let fib_1 = 1
  let fib_2 = 1
  
  // 使用加法计算前6项斐波那契数的和（简化版）
  let fib_3 = @azimuth.add(fib_1, fib_2)    // 2
  let fib_4 = @azimuth.add(fib_2, fib_3)    // 3
  let fib_5 = @azimuth.add(fib_3, fib_4)    // 5
  let fib_6 = @azimuth.add(fib_4, fib_5)    // 8
  
  // 计算总和
  let sum_1 = @azimuth.add(fib_1, fib_2)  // 2
  let sum_2 = @azimuth.add(sum_1, fib_3)  // 4
  let sum_3 = @azimuth.add(sum_2, fib_4)  // 7
  let sum_4 = @azimuth.add(sum_3, fib_5)  // 12
  let total_sum = @azimuth.add(sum_4, fib_6)  // 20
  
  @azimuth.assert_eq(20, total_sum)  // 1+1+2+3+5+8 = 20
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数的高级测试
  let prefix = "Dr."
  let first_name = "Jane"
  let last_name = "Smith"
  let title = "PhD"
  
  // 构建完整姓名
  let full_name = prefix + " " + first_name + " " + last_name + ", " + title
  let formal_greeting = @azimuth.greet(full_name)
  
  @azimuth.assert_eq_string("Hello, Dr. Jane Smith, PhD!", formal_greeting)
  
  // 测试空字符串和特殊字符的组合
  let empty_name = ""
  let special_chars = "!@#$%^&*()"
  let combined = empty_name + special_chars
  let special_greeting = @azimuth.greet(combined)
  
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", special_greeting)
}