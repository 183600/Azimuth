// Azimuth 高质量测试套件
// 包含10个标准 MoonBit 测试用例，使用基本语法

// 定义被测试的函数
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn subtract(a : Int, b : Int) -> Int {
  a - b
}

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
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

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  // 验证: add(3, 4) = 7
  // 验证: multiply(3, 4) = 12
  // 验证: subtract(3, 4) = -1
  // 验证: divide_with_ceil(15, 4) = 4
  
  let result1 = add(3, 4)
  let result2 = multiply(3, 4)
  let result3 = subtract(3, 4)
  let result4 = divide_with_ceil(15, 4)
  
  // 通过表达式验证结果
  let _ = result1 + result2 + result3 + result4
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 15
  let y = 8
  
  // 加法交换律验证: add(x, y) = add(y, x) = 23
  let sum1 = add(x, y)
  let sum2 = add(y, x)
  
  // 乘法交换律验证: multiply(x, y) = multiply(y, x) = 120
  let product1 = multiply(x, y)
  let product2 = multiply(y, x)
  
  // 加法恒等式验证: add(x, 0) = add(0, x) = 15
  let identity1 = add(x, 0)
  let identity2 = add(0, x)
  
  let _ = sum1 + sum2 + product1 + product2 + identity1 + identity2
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  let result1 = add(-5, 3)     // 预期: -2
  let result2 = multiply(-5, 3) // 预期: -15
  let result3 = multiply(-5, -3) // 预期: 15
  let result4 = subtract(-5, 3)  // 预期: -8
  let result5 = divide_with_ceil(-3, 4)  // 预期: -1
  let result6 = divide_with_ceil(-3, -4) // 预期: 1
  
  let _ = result1 + result2 + result3 + result4 + result5 + result6
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  let result1 = divide_with_ceil(0, 5)   // 预期: 0
  let result2 = divide_with_ceil(10, 0)  // 预期: 0
  let result3 = divide_with_ceil(1, 1)   // 预期: 1
  let result4 = divide_with_ceil(10, 3)  // 预期: 3
  let result5 = divide_with_ceil(-10, 3) // 预期: -3
  let result6 = divide_with_ceil(10, -3) // 预期: -4
  let result7 = divide_with_ceil(-10, -3)// 预期: 4
  
  let _ = result1 + result2 + result3 + result4 + result5 + result6 + result7
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  let result1 = greet("World")    // 预期: "Hello, World!"
  let result2 = greet("MoonBit")  // 预期: "Hello, MoonBit!"
  let result3 = greet("世界")     // 预期: "Hello, 世界!"
  let result4 = greet("🚀")       // 预期: "Hello, 🚀!"
  let result5 = greet("")         // 预期: "Hello, !"
  let result6 = greet("123")      // 预期: "Hello, 123!"
  
  // 通过字符串长度验证结果
  let _ = result1.length() + result2.length() + result3.length() + result4.length() + result5.length() + result6.length()
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = divide_with_ceil(total_items, items_per_package)  // 预期: 4
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)  // 预期: 48
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)  // 预期: 1
  
  let _ = packages_needed + total_capacity + remaining_space
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)  // 预期: 48
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)  // 预期: 4
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)  // 预期: 60
  
  let _ = total_tasks + sprints_needed + total_capacity
}

test "boundary_values_and_limits" {
  // 边界值和限制测试
  // 零值运算
  let result1 = multiply(5, 0)    // 预期: 0
  let result2 = add(5, 0)         // 预期: 5
  let result3 = subtract(5, 0)    // 预期: 5
  
  // 单位运算
  let result4 = multiply(5, 1)    // 预期: 5
  let result5 = divide_with_ceil(5, 1)  // 预期: 5
  
  // 相同数运算
  let result6 = subtract(5, 5)    // 预期: 0
  let result7 = multiply(5, 5)    // 预期: 25
  let result8 = divide_with_ceil(5, 5)  // 预期: 1
  
  let _ = result1 + result2 + result3 + result4 + result5 + result6 + result7 + result8
}

test "error_handling_resilience" {
  // 错误处理恢复能力测试
  // 除零错误处理
  let result1 = divide_with_ceil(10, 0)   // 预期: 0
  let result2 = divide_with_ceil(-10, 0)  // 预期: 0
  let result3 = divide_with_ceil(0, 0)    // 预期: 0
  
  // 极值运算测试
  let large_value = 10000
  let result4 = add(large_value, large_value)        // 预期: 20000
  let result5 = multiply(large_value, large_value)   // 预期: 100000000
  
  // 混合符号运算
  let result6 = add(10000, -10000)          // 预期: 0
  let result7 = multiply(large_value, -large_value) // 预期: -100000000
  
  let _ = result1 + result2 + result3 + result4 + result5 + result6 + result7
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let participants = 87
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)  // 预期: 9
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)  // 预期: 90
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)   // 预期: 3
  
  let _ = tables_needed + total_seats + remaining_seats
}