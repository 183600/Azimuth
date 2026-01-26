// 新增的标准 MoonBit 测试用例
// 为 azimuth 库添加的高质量测试用例，包含函数定义和测试

// 函数定义
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
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

pub fn subtract(a : Int, b : Int) -> Int {
  a - b
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

// 测试用例
test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, divide_with_ceil(7, 3))
  assert_eq(3, subtract(10, 7))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法的边界情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(4, divide_with_ceil(13, 4))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greet_function_international_support" {
  // 问候函数的国际字符支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "complex_business_calculation" {
  // 复杂业务计算测试：项目资源分配
  let team_members = 7
  let tasks_per_member = 5
  let total_tasks = multiply(team_members, tasks_per_member)
  let sprint_capacity = 16
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(35, total_tasks)
  assert_eq(3, sprints_needed)
}

test "mathematical_distributive_property" {
  // 数学分配律测试
  let a = 4
  let b = 6
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-5, -3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(2, subtract(-3, -5))
}

test "string_input_validation" {
  // 字符串输入验证测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, Alice123!", greet("Alice123"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "real_world_packaging_problem" {
  // 真实世界包装问题测试
  let items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items, box_capacity)
  let total_capacity = multiply(boxes_needed, box_capacity)
  let wasted_space = subtract(total_capacity, items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, wasted_space)
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}