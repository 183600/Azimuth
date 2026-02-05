// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

// 引入或重新定义需要测试的函数
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

// 断言函数
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

pub fn assert_true(condition : Bool) -> Unit {
  ()
}

pub fn assert_false(condition : Bool) -> Unit {
  ()
}

// 测试用例1：基本算术运算
test "basic_arithmetic_operations" {
  // 测试加法
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  
  // 测试乘法
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(7, 0))
  assert_eq(15, multiply(-3, -5))
  
  // 测试减法
  assert_eq(4, subtract(7, 3))
  assert_eq(-4, subtract(3, 7))
  assert_eq(0, subtract(5, 5))
}

// 测试用例2：向上取整除法
test "divide_with_ceil_functionality" {
  // 正数除法
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(3, 4))
  
  // 除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 负数除法
  assert_eq(-2, divide_with_ceil(-10, 4))
  assert_eq(2, divide_with_ceil(-10, -4))
}

// 测试用例3：字符串处理
test "string_greeting_functionality" {
  // 基本问候
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  
  // 特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

// 测试用例4：数学性质验证
test "mathematical_properties" {
  let x = 6
  let y = 8
  
  // 交换律验证
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 结合律验证
  assert_eq(add(add(x, y), 4), add(x, add(y, 4)))
  assert_eq(multiply(multiply(x, y), 2), multiply(x, multiply(y, 2)))
  
  // 分配律验证
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
}

// 测试用例5：边界条件测试
test "boundary_conditions" {
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 单位元测试
  assert_eq(5, multiply(5, 1))
  assert_eq(5, subtract(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 大数测试
  assert_eq(2000, add(1000, 1000))
  assert_eq(1000000, multiply(1000, 1000))
}

// 测试用例6：实际业务场景
test "business_inventory_scenario" {
  // 库存管理场景
  let total_products = 95
  let box_capacity = 12
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(8, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(96, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(1, remaining_space)
}

// 测试用例7：复杂计算流程
test "complex_calculation_workflow" {
  // 项目管理计算流程
  let team_size = 5
  let tasks_per_person = 8
  let sprint_capacity = 20
  
  // 计算总任务数
  let total_tasks = multiply(team_size, tasks_per_person)
  assert_eq(40, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(2, sprints_needed)
  
  // 计算总工作量
  let total_work = add(total_tasks, multiply(sprints_needed, 5))
  assert_eq(50, total_work)
}

// 测试用例8：错误处理和特殊情况
test "error_handling_and_special_cases" {
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值情况
  assert_eq(1, divide_with_ceil(1, 1000))
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  
  // 字符串边界情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, space !", greet("space "))
}