use marigold::m;
use marigold::marigold_impl::StreamExt;

/// Edge Case 1: Basic fold with literal initial value
async fn test_fold_literal_init() {
    println!("Testing: Fold with literal initial value");
    
    let result = m!(
        fn add(acc: u32, item: u32) -> u32 %%%MARIGOLD_FUNCTION_START%%%
            acc + item
        %%%MARIGOLD_FUNCTION_END%%%
        
        range(1, 4)  // [1, 2, 3] -> should sum to 6 + 0 = 6
            .fold(0, add)
            .return
    ).await.collect::<Vec<u32>>().await;
    
    assert_eq!(result, vec![6]);
    println!("✅ Basic fold with literal init: PASSED");
}

/// Edge Case 2: Fold with function initial value
async fn test_fold_function_init() {
    println!("Testing: Fold with function initial value");
    
    let result = m!(
        fn get_initial() -> u32 %%%MARIGOLD_FUNCTION_START%%%
            10
        %%%MARIGOLD_FUNCTION_END%%%
        
        fn multiply(acc: u32, item: u32) -> u32 %%%MARIGOLD_FUNCTION_START%%%
            acc * item
        %%%MARIGOLD_FUNCTION_END%%%
        
        range(1, 4)  // [1, 2, 3] -> should be 10 * 1 * 2 * 3 = 60
            .fold(get_initial, multiply)
            .return
    ).await.collect::<Vec<u32>>().await;
    
    assert_eq!(result, vec![60]);
    println!("✅ Fold with function init: PASSED");
}

/// Edge Case 3: Empty stream fold
async fn test_fold_empty_stream() {
    println!("Testing: Fold on empty stream");
    
    let result = m!(
        fn add(acc: i32, item: i32) -> i32 {
            acc + item
        }
        
        range(5, 5)  // Empty range
            .fold(42, add)
            .return
    ).await.collect::<Vec<i32>>().await;
    
    assert_eq!(result, vec![42]);  // Should return initial value
    println!("✅ Empty stream fold: PASSED");
}

/// Edge Case 4: Fold with pre-processing transformations
async fn test_fold_with_pre_transformations() {
    println!("Testing: Fold with pre-processing transformations");
    
    let result = m!(
        fn double(x: i32) -> i32 %%%MARIGOLD_FUNCTION_START%%%
            x * 2
        %%%MARIGOLD_FUNCTION_END%%%
        
        fn is_even(x: &i32) -> bool %%%MARIGOLD_FUNCTION_START%%%
            x % 2 == 0
        %%%MARIGOLD_FUNCTION_END%%%
        
        fn sum(acc: i32, item: i32) -> i32 %%%MARIGOLD_FUNCTION_START%%%
            acc + item
        %%%MARIGOLD_FUNCTION_END%%%
        
        range(1, 6)  // [1, 2, 3, 4, 5]
            .map(double)     // [2, 4, 6, 8, 10]
            .filter(is_even) // [2, 4, 6, 8, 10] (all even after doubling)
            .fold(0, sum)    // 2 + 4 + 6 + 8 + 10 = 30
            .return
    ).await.collect::<Vec<i32>>().await;
    
    assert_eq!(result, vec![30]);
    println!("✅ Fold with pre-transformations: PASSED");
}

/// Edge Case 5: Post-fold transformation (the failing case)
async fn test_fold_with_post_transformation() {
    println!("Testing: Fold with post-transformation");
    
    let result = m!(
        struct Container {
            value: u32
        }
        
        fn make_container() -> Container {
            Container { value: 0 }
        }
        
        fn accumulate(container: Container, item: u32) -> Container {
            let mut container = container;
            container.value += item;
            container
        }
        
        fn extract_value(container: Container) -> u32 {
            container.value
        }
        
        range(1, 4)  // [1, 2, 3] -> sum = 6
            .fold(make_container, accumulate)  // Container { value: 6 }
            .map(extract_value)               // 6
            .return
    ).await.collect::<Vec<u32>>().await;
    
    assert_eq!(result, vec![6]);
    println!("✅ Fold with post-transformation: PASSED");
}

/// Edge Case 6: Multiple post-fold transformations
async fn test_fold_with_multiple_post_transformations() {
    println!("Testing: Fold with multiple post-transformations");
    
    let result = m!(
        fn sum(acc: i32, item: i32) -> i32 {
            acc + item
        }
        
        fn double(x: i32) -> i32 {
            x * 2
        }
        
        fn add_ten(x: i32) -> i32 {
            x + 10
        }
        
        range(1, 4)  // [1, 2, 3] -> sum = 6
            .fold(0, sum)    // 6
            .map(double)     // 12
            .map(add_ten)    // 22
            .return
    ).await.collect::<Vec<i32>>().await;
    
    assert_eq!(result, vec![22]);
    println!("✅ Multiple post-fold transformations: PASSED");
}

/// Edge Case 7: Complex type fold
/*
async fn test_fold_complex_types() {
    println!("Testing: Fold with complex types");
    
    let result = m!(
        struct Stats {
            count: u32,
            sum: u32,
            max: u32
        }
        
        fn initial_stats() -> Stats %%%MARIGOLD_FUNCTION_START%%%
            Stats { count: 0, sum: 0, max: 0 }
        %%%MARIGOLD_FUNCTION_END%%%
        
        fn update_stats(mut stats: Stats, item: u32) -> Stats %%%MARIGOLD_FUNCTION_START%%%
            stats.count += 1;
            stats.sum += item;
            if item > stats.max {
                stats.max = item;
            }
            stats
        %%%MARIGOLD_FUNCTION_END%%%
        
        fn get_average(stats: Stats) -> u32 %%%MARIGOLD_FUNCTION_START%%%
            if stats.count > 0 {
                stats.sum / stats.count
            } else {
                0
            }
        %%%MARIGOLD_FUNCTION_END%%%
        
        range(1, 6)  // [1, 2, 3, 4, 5] -> average = 15/5 = 3
            .fold(initial_stats, update_stats)
            .map(get_average)
            .return
    ).await.collect::<Vec<u32>>().await;
    
    assert_eq!(result, vec![3]);
    println!("✅ Complex type fold: PASSED");
}
*/

/// Edge Case 8: Fold in pipeline with variables
async fn test_fold_with_variables() {
    println!("Testing: Fold with stream variables");
    
    // First test without variables to confirm it works
    println!("Debug: Testing direct range...");
    let simple_result = m!(
        range(1, 4).return
    ).await.collect::<Vec<i32>>().await;
    println!("Debug: Simple range result: {:?}", simple_result);
    
    println!("Debug: Testing variable range...");
    let var_result = m!(
        numbers = range(1, 4)
        numbers.return
    ).await.collect::<Vec<i32>>().await;
    println!("Debug: Variable range result: {:?}", var_result);
    
    println!("Debug: Testing direct range fold...");
    let direct_result = m!(
        fn multiply(acc: i32, item: i32) -> i32 %%%MARIGOLD_FUNCTION_START%%%
            acc * item
        %%%MARIGOLD_FUNCTION_END%%%
        
        range(1, 4)  // [1, 2, 3]
            .fold(1, multiply)  // 1 * 1 * 2 * 3 = 6
            .return
    ).await.collect::<Vec<i32>>().await;
    println!("Debug: Direct result: {:?}", direct_result);
    
    // Now test with variables
    println!("Debug: Testing variable-based fold...");
    let result = m!(
        fn multiply2(acc: i32, item: i32) -> i32 %%%MARIGOLD_FUNCTION_START%%%
            acc * item
        %%%MARIGOLD_FUNCTION_END%%%
        
        numbers = range(1, 4)  // [1, 2, 3]
        
        numbers
            .fold(1, multiply2)  // 1 * 1 * 2 * 3 = 6
            .return
    ).await.collect::<Vec<i32>>().await;
    
    println!("Debug: Variable result: {:?}", result);
    assert_eq!(result, vec![6]);
    println!("✅ Fold with variables: PASSED");
}

#[tokio::main]
async fn main() {
    println!("🧪 Running Fold Edge Case Test Suite");
    println!("=====================================");
    
    test_fold_literal_init().await;
    test_fold_function_init().await;
    test_fold_empty_stream().await;
    test_fold_with_pre_transformations().await;
    
    // These are the failing edge cases we need to fix:
    println!("\n🔧 Testing problematic edge cases:");
    
    // Try to run the problematic cases to see current status
    println!("🔧 Post-fold transformation tests temporarily disabled...");
    test_fold_with_post_transformation().await;
    test_fold_with_multiple_post_transformations().await;
    // test_fold_complex_types().await;
    test_fold_with_variables().await;
    
    println!("\n🎯 TDD Analysis Complete!");
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn edge_case_basic_fold() {
        test_fold_literal_init().await;
        test_fold_function_init().await;
        test_fold_empty_stream().await;
        test_fold_with_pre_transformations().await;
    }
    
    // These tests are expected to fail until we fix post-fold transformations
    #[tokio::test]
    async fn edge_case_post_fold_single() {
        test_fold_with_post_transformation().await;
    }
    
    #[tokio::test]
    async fn edge_case_post_fold_multiple() {
        test_fold_with_multiple_post_transformations().await;
    }
    
    // Complex types test is commented out in main
    // #[tokio::test]
    // #[should_panic]
    // async fn edge_case_complex_types() {
    //     test_fold_complex_types().await;
    // }
    
    #[tokio::test]
    async fn edge_case_variables() {
        test_fold_with_variables().await;
    }
}