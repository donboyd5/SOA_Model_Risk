


# Create a matrix with 100,000 rows and 50 columns
matrix <- matrix(rnorm(5000000), nrow = 100000, ncol = 50)

# Create a list of lists with 100,000 elements, each of which is a list with 50 elements
list <- replicate(100000, replicate(50, rnorm(1)))

# Measure the time it takes to iterate through the matrix
system.time(
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      x <- matrix[i, j]
    }
  }
)

# Measure the time it takes to iterate through the list of lists
system.time(
  for (i in 1:length(list)) {
    for (j in 1:length(list[[i]])) {
      x <- list[[i]][[j]]
    }
  }
)


with(mtcars, mpg[cyl == 8  &  disp > 350])
df

with(df,
     list(sum(default), mean(default))
  )

df


# Generate a correlation matrix ----
n <- 1200 # 1200 monthly observations, 100 years
set.seed(1234)
param <- .5
cor_matrix <- matrix(param, nrow = n, ncol = n)
diag(cor_matrix) <- 1

# Decompose the correlation matrix using Choleski decomposition
L <- chol(cor_matrix)
# dim(L)

# Generate a set of uncorrelated random samples
x <- rnorm(n)

# Transform the uncorrelated samples using the Choleski decomposition
y <- L %*% x

cor(y, x)

# The y vector now contains correlated random samples

tibble(x=x, y=y) |> 
  ggplot(aes(x, y)) +
  geom_point(colour="blue")


# next section ------------------------------------------------------------




