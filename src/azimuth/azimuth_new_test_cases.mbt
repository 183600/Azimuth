// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行补充测试

test "extreme_boundary_addition" {
  // 测试极端边界值的加法
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近Int最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数
  assert_eq(1, add(-2147483648, 2147483647))  // 最小值与最大值-1
}

test "divide_with_ceil_complex_scenarios" {
  // 测试复杂的向上取整除法场景
  assert_eq(1, divide_with_ceil(1, 2147483647))  // 极大除数
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))  // 极大商
  assert_eq(-1, divide_with_ceil(-1, 2147483647))  // 负数极小商
  assert_eq(0, divide_with_ceil(2147483647, -2147483648))  // 正负边界
}

test "greet_function_edge_unicode" {
  // 测试问候函数的边缘Unicode字符
  assert_eq_string("Hello, \n!", greet("\n"))  // 换行符
  assert_eq_string("Hello, \t!", greet("\t"))  // 制表符
  assert_eq_string("Hello, \r!", greet("\r"))  // 回车符
  assert_eq_string("Hello, 🌈!", greet("🌈"))  // 彩虹emoji
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  let y = 17
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 零性质
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "nested_function_composition" {
  // 测试嵌套函数组合
  let base = 10
  
  // 复杂嵌套：multiply(add(divide_with_ceil(multiply(base, 3), 4), 5), 2)
  let step1 = multiply(base, 3)  // 30
  let step2 = divide_with_ceil(step1, 4)  // 8
  let step3 = add(step2, 5)  // 13
  let result = multiply(step3, 2)  // 26
  
  assert_eq(30, step1)
  assert_eq(8, step2)
  assert_eq(13, step3)
  assert_eq(26, result)
}

test "real_world_financial_calculation" {
  // 实际金融计算：贷款分期付款
  let loan_amount = 10000
  let interest_rate = 5  // 5%
  let months = 24
  
  // 计算总利息（简化模型）
  let total_interest = divide_with_ceil(multiply(loan_amount, multiply(interest_rate, months)), 1200)
  
  // 计算月供（向上取整）
  let monthly_payment = divide_with_ceil(add(loan_amount, total_interest), months)
  
  // 计算总还款额
  let total_payment = multiply(monthly_payment, months)
  
  assert_eq(1000, total_interest)  // 10000 * 5% * 24 / 12 = 1000
  assert_eq(459, monthly_payment)  // ceil(11000 / 24) = 459
  assert_eq(11016, total_payment)  // 459 * 24 = 11016
}

test "algorithm_complexity_simulation" {
  // 模拟算法复杂度计算
  let n = 100
  let constant_factor = 3
  
  // O(n) 算法
  let linear_time = multiply(n, constant_factor)
  
  // O(n²) 算法（简化计算）
  let quadratic_time = multiply(multiply(n, n), constant_factor) / 100
  
  // O(log n) 算法（简化为向上取整除法）
  let logarithmic_time = divide_with_ceil(n, 10)
  
  assert_eq(300, linear_time)
  assert_eq(300, quadratic_time)
  assert_eq(10, logarithmic_time)
}

test "string_processing_with_numbers" {
  // 测试字符串与数字的处理
  let number_as_string = "12345"
  let processed_greeting = greet("User_" + number_as_string)
  
  assert_eq_string("Hello, User_12345!", processed_greeting)
  
  // 测试特殊字符组合
  let special_chars = "!@#$%^&*()"
  assert_eq_string("Hello, !@#$%^&*()!", greet(special_chars))
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_resources = 100
  let project_requirements = [15, 25, 30, 20]
  
  // 计算总需求
  let total_required = add(add(add(project_requirements[0], project_requirements[1]), project_requirements[2]), project_requirements[3])
  
  // 计算剩余资源
  let remaining = add(total_resources, -total_required)
  
  // 计算可以支持的额外项目数（每个项目需要10个资源）
  let additional_projects = divide_with_ceil(remaining, 10)
  
  assert_eq(90, total_required)  // 15 + 25 + 30 + 20 = 90
  assert_eq(10, remaining)  // 100 - 90 = 10
  assert_eq(1, additional_projects)  // ceil(10 / 10) = 1
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 测试除数为零的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试极值运算
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(0, multiply(-2147483648, 0))
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, multiply(0, -2147483648))
}