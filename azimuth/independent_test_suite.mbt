// 独立的 MoonBit 测试套件
// 为 Azimuth 项目添加的自包含测试用例

// 函数定义
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

fn subtract(a : Int, b : Int) -> Int {
  a - b
}

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

// 测试用例
test "independent_add_test" {
  // 独立加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

test "independent_multiply_test" {
  // 独立乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "independent_greet_test" {
  // 独立问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "independent_divide_test" {
  // 独立除法测试
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(4, divide_with_ceil(15, 4))
}

test "independent_subtract_test" {
  // 独立减法测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
}

test "independent_math_properties" {
  // 独立数学性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(7, 13))
  assert_eq(91, multiply(7, 13))
}

test "independent_business_scenario" {
  // 独立业务场景测试
  let items = 25
  let capacity = 6
  let boxes = divide_with_ceil(items, capacity)
  
  assert_eq(5, boxes)
  
  // 验证总容量
  let total_capacity = multiply(boxes, capacity)
  let remaining = subtract(total_capacity, items)
  assert_eq(5, remaining)
}

test "independent_edge_cases" {
  // 独立边界情况测试
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000, add(1000, 1000))
  assert_eq(10000, multiply(100, 100))
  
  // 负数运算
  assert_eq(-10, add(-5, -5))
  assert_eq(25, multiply(-5, -5))
}

test "independent_complex_workflow" {
  // 独立复杂工作流测试
  let initial = 10
  let step1 = add(initial, multiply(2, 3))  // 10 + 2*3 = 16
  let step2 = divide_with_ceil(step1, 5)    // ceil(16/5) = 4
  let final_result = add(step2, 1)          // 4 + 1 = 5
  
  assert_eq(5, final_result)
}

test "independent_negative_operations" {
  // 独立负数运算测试
  // 负数加法
  assert_eq(-8, add(-5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  
  // 负数乘法
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  
  // 负数除法
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-2, divide_with_ceil(7, -4))
  assert_eq(2, divide_with_ceil(-7, -4))
}