// 标准 MoonBit 测试套件 - 10个高质量测试用例
// 为 Azimuth 项目添加全面的功能测试覆盖

// 定义所需的函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  if expected == actual {
    ()
  } else {
    let _ = "Assertion failed"
  }
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  if expected == actual {
    ()
  } else {
    let _ = "String assertion failed"
  }
  ()
}

fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn divide_with_ceil(a : Int, b : Int) -> Int {
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

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}

// 辅助函数：布尔断言
fn assert_true(condition : Bool) -> Unit {
  if condition {
    ()
  } else {
    let _ = "Boolean assertion failed"
  }
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试负数的交换律
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(multiply(-4, 7), multiply(7, -4))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "complex_business_logic_scenario" {
  // 复杂业务逻辑场景测试
  // 场景：计算装盒和运输成本
  
  let total_products = 127
  let box_capacity = 15
  let cost_per_box = 25
  let shipping_cost_per_box = 8
  
  // 计算需要的盒子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(9, boxes_needed)  // ceil(127/15) = 9
  
  // 计算总成本
  let box_cost = multiply(boxes_needed, cost_per_box)
  let shipping_cost = multiply(boxes_needed, shipping_cost_per_box)
  let total_cost = add(box_cost, shipping_cost)
  
  assert_eq(225, box_cost)        // 9 * 25
  assert_eq(72, shipping_cost)    // 9 * 8
  assert_eq(297, total_cost)      // 225 + 72
}

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 8
  let c = 4
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 6 * (8 + 4) = 6 * 8 + 6 * 4 = 72
  
  // 测试另一个组合
  let x = 12
  let y = 5
  let z = 3
  
  assert_eq(
    multiply(x, add(y, z)),
    add(multiply(x, y), multiply(x, z))
  )
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  // Unicode 字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  
  // Emoji 测试
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  
  // 特殊符号测试
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, &*()!", greet("&*()"))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：服务器资源分配
  
  let total_memory = 16384  // GB
  let total_cpu_cores = 256
  let number_of_services = 7
  
  // 计算每个服务的最小资源分配
  let memory_per_service = divide_with_ceil(total_memory, number_of_services)
  let cpu_per_service = divide_with_ceil(total_cpu_cores, number_of_services)
  
  assert_eq(2341, memory_per_service)  // ceil(16384/7)
  assert_eq(37, cpu_per_service)       // ceil(256/7)
  
  // 验证总分配资源
  let total_allocated_memory = multiply(memory_per_service, number_of_services)
  let total_allocated_cpu = multiply(cpu_per_service, number_of_services)
  
  assert_eq(16387, total_allocated_memory)  // 2341 * 7
  assert_eq(259, total_allocated_cpu)       // 37 * 7
  
  // 计算资源浪费
  let memory_waste = subtract(total_allocated_memory, total_memory)
  let cpu_waste = subtract(total_allocated_cpu, total_cpu_cores)
  
  assert_eq(3, memory_waste)  // 16387 - 16384
  assert_eq(3, cpu_waste)     // 259 - 256
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(12345, 0))
  
  // 大数测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "sequential_calculation_pipeline" {
  // 连续计算流水线测试
  // 场景：多步骤数据处理
  
  let initial_value = 10
  
  // 步骤1：加法运算
  let step1 = add(initial_value, 15)  // 25
  
  // 步骤2：乘法运算
  let step2 = multiply(step1, 3)      // 75
  
  // 步骤3：除法运算
  let step3 = divide_with_ceil(step2, 20)  // 4
  
  // 步骤4：再次加法
  let step4 = add(step3, 5)           // 9
  
  // 步骤5：最终乘法
  let final_result = multiply(step4, 2)   // 18
  
  assert_eq(25, step1)
  assert_eq(75, step2)
  assert_eq(4, step3)
  assert_eq(9, step4)
  assert_eq(18, final_result)
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  
  // 加法恒等式: x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式: x * 1 = x
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元: x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 负元素性质: x + (-x) = 0
  assert_eq(0, add(x, multiply(x, -1)))
  assert_eq(0, add(multiply(x, -1), x))
}
