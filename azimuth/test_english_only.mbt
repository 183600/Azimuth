// Azimuth High Quality Test Suite - 10 standard MoonBit test cases
// Covering various scenarios and edge cases of core functionality

test "arithmetic_operations_comprehensive" {
  // Comprehensive arithmetic operations test
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(5, -6))
  assert_eq(0, add(-3, 3))
  assert_eq(100, multiply(10, 10))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(0, multiply(0, 123))
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
}

test "divide_with_ceil_edge_cases" {
  // Edge cases for ceiling division
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-2, divide_with_ceil(-3, 2))
  assert_eq(-1, divide_with_ceil(3, -2))
}

test "greet_function_internationalization" {
  // Internationalization test for greeting function
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, world!", greet("world"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_validation" {
  // Mathematical properties validation test
  let x = 12
  let y = 8
  
  // Commutative property of addition
  assert_eq(add(x, y), add(y, x))
  
  // Commutative property of multiplication
  assert_eq(multiply(x, y), multiply(y, x))
  
  // Distributive property
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // Inverse operation of subtraction
  assert_eq(x, add(subtract(x, y), y))
}

test "business_logic_packaging_optimization" {
  // Business logic: packaging optimization test
  let total_items = 47
  let box_capacity = 12
  
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(4, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  let wasted_space = subtract(total_capacity, total_items)
  assert_eq(1, wasted_space)
  
  let space_utilization = multiply(divide_with_ceil(total_items, total_capacity), 100)
  assert_eq(98, space_utilization)
}

test "complex_calculation_pipeline" {
  // Complex calculation pipeline test
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let offset = 3
  
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, offset)
  
  assert_eq(12, result)  // ceil(15*4/7) + 3 = 9 + 3 = 12
}

test "error_handling_robustness" {
  // Error handling robustness test
  // Division by zero error handling
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // Extreme value handling
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
}

test "resource_allocation_scenario" {
  // Resource allocation scenario test
  let total_resources = 100
  let projects = 3
  let min_resources_per_project = divide_with_ceil(total_resources, projects)
  
  assert_eq(34, min_resources_per_project)
  
  let total_allocated = multiply(min_resources_per_project, projects)
  assert_eq(102, total_allocated)
  
  let resource_overhead = subtract(total_allocated, total_resources)
  assert_eq(2, resource_overhead)
}

test "performance_benchmark_operations" {
  // Performance benchmark operations test
  let large_number = 10000
  
  let sum_result = add(large_number, large_number)
  let product_result = multiply(large_number, 100)
  let division_result = divide_with_ceil(product_result, large_number)
  
  assert_eq(20000, sum_result)
  assert_eq(1000000, product_result)
  assert_eq(100, division_result)
}

test "real_world_application_comprehensive" {
  // Real-world application comprehensive test
  // Scenario: event planning and resource management
  let participants = 87
  let chairs_per_table = 10
  let budget_per_table = 50
  
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  let total_chairs = multiply(tables_needed, chairs_per_table)
  assert_eq(90, total_chairs)
  
  let empty_chairs = subtract(total_chairs, participants)
  assert_eq(3, empty_chairs)
  
  let total_budget = multiply(tables_needed, budget_per_table)
  assert_eq(450, total_budget)
  
  let budget_per_person = divide_with_ceil(total_budget, participants)
  assert_eq(6, budget_per_person)
}