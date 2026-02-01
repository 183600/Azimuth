// Azimuth 项目新增标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例
// 使用内置断言方法

test "arithmetic_commutative_laws" {
  // 测试算术交换律
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  let add1 = a + b
  let add2 = b + a
  if (add1 != add2) {
    @builtin.abort("Addition commutative law failed")
  }
  
  // 乘法交换律: a * b = b * a
  let mul1 = a * b
  let mul2 = b * a
  if (mul1 != mul2) {
    @builtin.abort("Multiplication commutative law failed")
  }
  
  // 验证具体数值
  if (add1 != 42) {
    @builtin.abort("Expected 42, got " + add1.to_string())
  }
  if (mul1 != 405) {
    @builtin.abort("Expected 405, got " + mul1.to_string())
  }
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试 - 手动实现
  let divide_with_ceil = fn(a : Int, b : Int) -> Int {
    if b == 0 { 
      0 
    } else {
      let quotient = a / b
      let remainder = a % b
      if remainder == 0 {
        quotient
      } else if a > 0 && b > 0 {
        quotient + 1
      } else if a < 0 && b < 0 {
        quotient + 1
      } else {
        quotient
      }
    }
  }
  
  // 正数情况
  if (divide_with_ceil(10, 3) != 4) {
    @builtin.abort("Expected 4, got " + divide_with_ceil(10, 3).to_string())
  }
  if (divide_with_ceil(9, 3) != 3) {
    @builtin.abort("Expected 3, got " + divide_with_ceil(9, 3).to_string())
  }
  if (divide_with_ceil(1, 100) != 1) {
    @builtin.abort("Expected 1, got " + divide_with_ceil(1, 100).to_string())
  }
  
  // 负数情况
  if (divide_with_ceil(-10, 3) != -3) {
    @builtin.abort("Expected -3, got " + divide_with_ceil(-10, 3).to_string())
  }
  if (divide_with_ceil(10, -3) != -4) {
    @builtin.abort("Expected -4, got " + divide_with_ceil(10, -3).to_string())
  }
  if (divide_with_ceil(-10, -3) != 4) {
    @builtin.abort("Expected 4, got " + divide_with_ceil(-10, -3).to_string())
  }
  
  // 边界情况
  if (divide_with_ceil(0, 5) != 0) {
    @builtin.abort("Expected 0, got " + divide_with_ceil(0, 5).to_string())
  }
  if (divide_with_ceil(5, 0) != 0) {
    @builtin.abort("Expected 0, got " + divide_with_ceil(5, 0).to_string())
  }
  if (divide_with_ceil(5, 5) != 1) {
    @builtin.abort("Expected 1, got " + divide_with_ceil(5, 5).to_string())
  }
}

test "greet_function_internationalization" {
  // 问候函数国际化测试 - 手动实现
  let greet = fn(name : String) -> String {
    "Hello, " + name + "!"
  }
  
  let result1 = greet("MoonBit")
  if (result1 != "Hello, MoonBit!") {
    @builtin.abort("Expected 'Hello, MoonBit!', got '" + result1 + "'")
  }
  
  let result2 = greet("世界")
  if (result2 != "Hello, 世界!") {
    @builtin.abort("Expected 'Hello, 世界!', got '" + result2 + "'")
  }
  
  let result3 = greet("🚀")
  if (result3 != "Hello, 🚀!") {
    @builtin.abort("Expected 'Hello, 🚀!', got '" + result3 + "'")
  }
  
  let result4 = greet("café")
  if (result4 != "Hello, café!") {
    @builtin.abort("Expected 'Hello, café!', got '" + result4 + "'")
  }
  
  let result5 = greet("")
  if (result5 != "Hello, !") {
    @builtin.abort("Expected 'Hello, !', got '" + result5 + "'")
  }
  
  let result6 = greet("123")
  if (result6 != "Hello, 123!") {
    @builtin.abort("Expected 'Hello, 123!', got '" + result6 + "'")
  }
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 42
  
  // 加法恒等式: x + 0 = x
  if (x + 0 != x) {
    @builtin.abort("Addition identity property failed")
  }
  if (0 + x != x) {
    @builtin.abort("Addition identity property failed")
  }
  
  // 乘法恒等式: x * 1 = x, x * 0 = 0
  if (x * 1 != x) {
    @builtin.abort("Multiplication identity property failed")
  }
  if (1 * x != x) {
    @builtin.abort("Multiplication identity property failed")
  }
  if (x * 0 != 0) {
    @builtin.abort("Multiplication zero property failed")
  }
  
  // 减法恒等式: x - x = 0, x - 0 = x
  if (x - x != 0) {
    @builtin.abort("Subtraction identity property failed")
  }
  if (x - 0 != x) {
    @builtin.abort("Subtraction identity property failed")
  }
}

test "business_logistics_calculation" {
  // 物流业务计算测试
  let divide_with_ceil = fn(a : Int, b : Int) -> Int {
    if b == 0 { 
      0 
    } else {
      let quotient = a / b
      let remainder = a % b
      if remainder == 0 {
        quotient
      } else if a > 0 && b > 0 {
        quotient + 1
      } else if a < 0 && b < 0 {
        quotient + 1
      } else {
        quotient
      }
    }
  }
  
  let total_packages = 157
  let truck_capacity = 24
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  // 验证需要的卡车数量
  if (trucks_needed != 7) {
    @builtin.abort("Expected 7 trucks, got " + trucks_needed.to_string())
  }
  
  // 验证总运力
  let total_capacity = trucks_needed * truck_capacity
  if (total_capacity != 168) {
    @builtin.abort("Expected 168 total capacity, got " + total_capacity.to_string())
  }
  
  // 验证剩余运力
  let remaining_capacity = total_capacity - total_packages
  if (remaining_capacity != 11) {
    @builtin.abort("Expected 11 remaining capacity, got " + remaining_capacity.to_string())
  }
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：计算项目成本和资源分配
  let team_size = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 12
  
  // 计算总工时
  let total_hours = team_size * (hours_per_day * project_days)
  if (total_hours != 768) {
    @builtin.abort("Expected 768 total hours, got " + total_hours.to_string())
  }
  
  // 计算总成本
  let total_cost = total_hours * hourly_rate
  if (total_cost != 57600) {
    @builtin.abort("Expected 57600 total cost, got " + total_cost.to_string())
  }
  
  // 计算支付周期（每周期不超过10000）
  let divide_with_ceil = fn(a : Int, b : Int) -> Int {
    if b == 0 { 
      0 
    } else {
      let quotient = a / b
      let remainder = a % b
      if remainder == 0 {
        quotient
      } else if a > 0 && b > 0 {
        quotient + 1
      } else if a < 0 && b < 0 {
        quotient + 1
      } else {
        quotient
      }
    }
  }
  
  let payment_limit = 10000
  let payment_periods = divide_with_ceil(total_cost, payment_limit)
  if (payment_periods != 6) {
    @builtin.abort("Expected 6 payment periods, got " + payment_periods.to_string())
  }
}

test "edge_cases_and_boundary_values" {
  // 边界值和边缘情况测试
  // 大数运算
  if (1000000 + 1000000 != 2000000) {
    @builtin.abort("Large number addition failed")
  }
  if (10000 * 10000 != 100000000) {
    @builtin.abort("Large number multiplication failed")
  }
  
  // 极值除法
  let divide_with_ceil = fn(a : Int, b : Int) -> Int {
    if b == 0 { 
      0 
    } else {
      let quotient = a / b
      let remainder = a % b
      if remainder == 0 {
        quotient
      } else if a > 0 && b > 0 {
        quotient + 1
      } else if a < 0 && b < 0 {
        quotient + 1
      } else {
        quotient
      }
    }
  }
  
  if (divide_with_ceil(999999, 1000000) != 1) {
    @builtin.abort("Extreme division case 1 failed")
  }
  if (divide_with_ceil(1000000, 1) != 1000000) {
    @builtin.abort("Extreme division case 2 failed")
  }
  
  // 零值运算
  if (123456 * 0 != 0) {
    @builtin.abort("Zero multiplication failed")
  }
  if (123456 + 0 != 123456) {
    @builtin.abort("Zero addition failed")
  }
  if (123456 - 0 != 123456) {
    @builtin.abort("Zero subtraction failed")
  }
  
  // 特殊问候
  let greet = fn(name : String) -> String {
    "Hello, " + name + "!"
  }
  
  let result1 = greet("   ")
  if (result1 != "Hello,   !") {
    @builtin.abort("Space greeting failed")
  }
  
  let result2 = greet("special@#$%")
  if (result2 != "Hello, special@#$%!") {
    @builtin.abort("Special character greeting failed")
  }
}

test "algorithm_complexity_validation" {
  // 算法复杂度验证测试
  // 验证不同算法路径的计算结果
  let input = 20
  
  // 路径1: 直接计算
  let result1 = input * (input + 1)
  
  // 路径2: 分步计算
  let temp = input + 1
  let result2 = input * temp
  
  // 验证结果一致性
  if (result1 != result2) {
    @builtin.abort("Algorithm path consistency failed")
  }
  if (result1 != 420) {
    @builtin.abort("Expected 420, got " + result1.to_string())
  }
  
  // 验证分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 2
  let distributive_left = a * (b + c)
  let distributive_right = a * b + a * c
  if (distributive_left != distributive_right) {
    @builtin.abort("Distributive property failed")
  }
  if (distributive_left != 25) {
    @builtin.abort("Expected 25, got " + distributive_left.to_string())
  }
}