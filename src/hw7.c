#include "hw7.h"
#include <string.h>

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (root == NULL) {
        //creating new node
        bst_sf *node = malloc(sizeof(bst_sf));
        node->mat = mat;
        node->left_child = NULL;
        node->right_child = NULL;
        
        return node;
    }

    //bst property
    if (mat->name < root->mat->name) {
        root->left_child = insert_bst_sf(mat, root->left_child);
    }
    else{
        root->right_child = insert_bst_sf(mat, root->right_child);
    }
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    //if not in bst or base case
    if (root == NULL) {
        return NULL;
    }

    //if found
    if (name = root->mat->name) {
        return root->mat;
    }
    //recursive calls
    else if(name < root->mat->name) {
        return find_bst_sf(name, root->left_child);
    }
    else {
        return find_bst_sf(name, root->right_child);
    }

}

void free_bst_sf(bst_sf *root) {
    if (root == NULL) {
        return;
    }

    //free subtrees 
    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    
    //free root
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    //size of matrix
    int r = mat1->num_rows;
    int c = mat1->num_cols;

    //creating res (result) matrix 'a' (add)
    matrix_sf *res = malloc(sizeof(matrix_sf) + r*c*sizeof(int));
    res->name = 'a';
    res->num_rows = r;
    res->num_cols = c;

    //adding the values and storing in res
    for (int i=0; i<r*c; i++) {
        res-> values[i] = mat1->values[i] + mat2->values[i];
    }

    return res;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    //size of matrices
    int m = mat1->num_rows;
    int n = mat1->num_cols;
    int r = mat2->num_cols;
   
    //creating res (result) matrix 'm' (multipy); dim = [(m1.row)*(m2.col)]
    matrix_sf *res = malloc(sizeof(matrix_sf) + m*r*sizeof(int));
    res->name = 'm';
    res->num_rows = m;
    res->num_cols = r;

    for (int i=0; i<m*r; i++) {
        res->values[i] = 0;
    }

    //cross product 
    for (int i=0; i<m; i++) {
        for (int j=0; j<r; j++) {
            int sum = 0;
            for (int k=0; k<n; k++) {
                sum += (mat1->values[i*n+k] * mat2->values[k*r+j]);
            }
            res->values[i*r+j] = sum;
        }
    }

    return res;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    //size of matrix
    int r = mat->num_rows;
    int c = mat->num_cols;

    //creating res (result) matrix 't' (transpose)
    matrix_sf *res = malloc(sizeof(matrix_sf) + r*c*sizeof(int));
    res->name = 't';
    res->num_rows = r;
    res->num_cols = c;

    //transposing the matrix 
    for (int i=0; i<r; i++) {
        for(int j=0; j<c; j++) {
            res->values[j*c+i] = mat->values[i*c+j];
        }
    }
    return res;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    
    const *p = expr;
    int r, c;
    int i=1;

    r = &p;
    c = &p+i;
    i++;

    matrix_sf *m = malloc(sizeof(matrix_sf) + r*c*sizeof(int));
    m->name = name;
    m->num_rows = r;
    m->num_cols = c;

    int j=0;
    while (*p && (i<r*c)) {
        int next = &p+i;
        i++;
        m->values[j++] = next;
    }

    
    return m;
}

//gives precedence of the operation
int prec(char op) {
    if (op == '+') {
        return 1;
    }
    if (op == '*') {
        return 2;
    }
    if (op == '\'') {
        return 1;
    }
    return 0;
}

char* infix2postfix_sf(char *infix) {
    size_t len = strlen(infix);
    char *res = malloc(len);

    // creating stack
    char stk[len];
    int top = -1;
    int out = 0;

    for (size_t i=0; i<len; i++) {
        char ch = infix[i];
        if (ch == '(') {
            stk[++top] = ch;
        }
        //operand
        else if (isupper(ch)) {
            res[out++] = ch;
        }
        //operator
        else if( (ch == '+') || (ch == '*') || (ch == '\'') ){
            if (ch == '\'') {
                stk[++top] = ch;
            }
            else {
                while ((top >= 0) && 
                ((stk[top] == '+') || (stk[top] == '*') || (stk[top] == '\'') )
                && prec(stk[top]) >= prec(ch)) {
                    res[out++] = stk[top];
                    top--;
                }
                stk[++top] = ch;
            }
        }
        else if(ch == ')') {
            while ( (top >= 0) && (stk[top] != '(')) {
                res[out++] = stk[top];
                top--;
            }
            
        }
        
    }

    //remaining char
    while (top >= 0) {
        res[out++] = stk[top];
        top--;
    }
    //null char 
    res[out] = '\0';
    return res;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *post = infix2postfix_sf(expr);
    if (!post) {
        return NULL;
    }

    matrix_sf *stk[300];
    int top = -1;

    for (char *p = post; *p != NULL; p++) {
        if (isUpper(*p)) {
            stk[++top] = find_bst_sf(*p, root);
        }
        else if (*p == '\'') {
            matrix_sf *m = stk[top--];
            matrix_sf *t = transpose_mat_sf(m);
            t->name = 't';
            stk[++top] = t;
        }
        else if((*p == '+') || (*p == '*') ) {
            matrix_sf *m1 = stk[top--];
            matrix_sf *m2 = stk[top--];
            matrix_sf *op;
            op->name = 'o';
            if (*p == '+') {
                op = add_mats_sf(m1, m2);
            }
            else{
                op = mult_mats_sf(m1, m2);
            }
            stk[++top] = op;
        }
    }

    matrix_sf *res = stk[top];
    res->name = name;

    free(post);

    return res;
}

matrix_sf *execute_script_sf(char *filename) {
    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
        return NULL;
    }

    char *line = NULL;
    
    return NULL;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
