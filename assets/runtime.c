#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum
{
    N_INT,
    N_APP,
    N_GLOBAL,
    N_IND,
    N_DATA
} node_tag;

typedef struct node node;

typedef struct node_app
{
    node *left;
    node *right;
} node_app;

typedef struct node_data
{
    int constructor;
    int arity;
    node *args;
} node_data;

// forward definition
struct stack;

typedef struct node_global
{
    int arity;
    struct stack *(*function)(struct stack *);
} node_global;

typedef struct node
{
    node_tag tag;
    union
    {
        int nint;
        node_app napp;
        node_global nglobal;
        struct node *nind;
        node_data ndata;
    };
} node;

node *new_node()
{
    return malloc(sizeof(node));
}

node *new_data_args(arity)
{
    return malloc(arity * sizeof(node));
}

typedef struct stack
{
    node *node;
    struct stack *next;
} stack;
stack *empty_stack()
{
    return 0;
}

bool is_stack_empty(stack *stack)
{
    return stack == 0;
}

node *peek(stack *stack)
{
    return stack->node;
}

stack *push(stack *stack, node *node)
{
    struct stack *new_stack = malloc(sizeof(stack));
    new_stack->next = stack;
    new_stack->node = node;
    return new_stack;
}

node *lookup(stack *stack, int offset)
{
    for (int i = 0; i < offset; i++)
    {
        stack = stack->next;
    }
    return stack->node;
}

stack *pop(stack *stack, int n)
{
    for (int i = 0; i < n; i++)
    {
        struct stack *old_stack = stack;
        stack = stack->next;
        // free(old_stack);
    }
    return stack;
}

stack *slide(stack *stack, int n)
{
    node *node = peek(stack);
    stack = pop(stack, n + 1);
    return push(stack, node);
}

void print_node(node *node)
{
    switch (node->tag)
    {
    case N_INT:
        printf("%d", node->nint);
        break;
    case N_APP:
        printf("N_APP");
        printf(" (");
        print_node(node->napp.left);
        printf(") (");
        print_node(node->napp.right);
        printf(")");
        break;
    case N_DATA:
        printf("N_DATA");
        printf(" {%d}", node->ndata.constructor);
        struct node *ptr = node->ndata.args;
        for (int i = 0; i < node->ndata.arity; i++)
        {
            printf(" (");
            print_node(ptr);
            ptr++;
            printf(")");
        }
        break;
    case N_GLOBAL:
        printf("N_GLOBAL {%p}", node);
        break;
    case N_IND:
        printf("N_IND {%p}", node);
        printf(" (");
        print_node(node->nind);
        printf(")");
        break;
    }
}

void print_stack(stack *stack, char *label)
{
    // return;
    printf("%s:\n", label);

    while (!is_stack_empty(stack))
    {
        print_node(peek(stack));
        printf("\n");
        stack = stack->next;
    }

    printf("\n");
}

/* INSTRUCTIONS */

stack *push_int(stack *stack, int n)
{
    node *node = new_node();
    node->tag = N_INT;
    node->nint = n;

    return push(stack, node);
}

stack *push_global(stack *stack, int arity, struct stack *(*function)(struct stack *))
{
    node *node = new_node();
    node->tag = N_GLOBAL;
    node->nglobal.arity = arity;
    node->nglobal.function = function;

    return push(stack, node);
}

stack *push_instr(stack *stack, int offset)
{
    node *node = lookup(stack, offset);
    return push(stack, node);
}

stack *mk_app(stack *stack)
{
    node *left = peek(stack);
    stack = pop(stack, 1);
    node *right = peek(stack);
    stack = pop(stack, 1);

    node *app = new_node();
    app->tag = N_APP;
    app->napp.left = left;
    app->napp.right = right;

    return push(stack, app);
}

stack *pack(stack *stack, int constr_tag, int n)
{
    node *node = new_node();
    node->tag = N_DATA;
    node->ndata.constructor = constr_tag;
    node->ndata.args = new_data_args(n);
    node->ndata.arity = n;

    struct node *ptr = node->ndata.args;
    for (int i = 0; i < n; i++)
    {
        struct node *arg = peek(stack);
        stack = pop(stack, 1);
        *ptr = *arg;
        ptr++;
    }

    return push(stack, node);
}

stack *split(stack *stack, int n)
{
    node *node = peek(stack);
    stack = pop(stack, 1);

    struct node *ptr = node->ndata.args + n - 1;
    for (int i = 0; i < n; i++)
    {
        struct node *arg = ptr;
        stack = push(stack, arg);
        ptr--;
    }

    return stack;
}

stack *update(stack *stack, int n)
{
    node *top_node = peek(stack);
    stack = pop(stack, 1);
    node *node_to_be_updated = lookup(stack, n);
    node_to_be_updated->tag = N_IND;
    node_to_be_updated->nind = top_node;

    return stack;
}

stack *unwind(stack *stack)
{
    while (1)
    {
        print_stack(stack, "UNWIND");
        node *node = peek(stack);
        switch (node->tag)
        {
        case N_APP:
            stack = push(stack, node->napp.left);
            break;
        case N_GLOBAL:
        {
            // pop the node_global
            struct node *root = peek(stack);
            stack = pop(stack, 1);

            struct stack *alt_stack = empty_stack();
            for (int i = 0; i < node->nglobal.arity; i++)
            {
                assert(stack != empty_stack()); // check enough arguments given
                struct node *node = peek(stack);
                root = node;
                stack = pop(stack, 1);
                alt_stack = push(alt_stack, node);
            }

            stack = push(stack, root);

            for (int i = 0; i < node->nglobal.arity; i++)
            {
                struct node *app = peek(alt_stack);
                alt_stack = pop(alt_stack, 1);
                assert(app->tag == N_APP);
                struct node *right = app->napp.right;
                stack = push(stack, right);
            }
            print_stack(stack, "BEFORE FUNCTION EXECUTION");
            stack = node->nglobal.function(stack);
            print_stack(stack, "AFTER FUNCTION EXECUTION");
            break;
        }
        case N_IND:
        {
            struct node *new_node = node->nind;
            stack = pop(stack, 1);
            stack = push(stack, new_node);
            break;
        }
        default:
            print_stack(stack, "RETURNING");
            return stack;
        }
    }
}

stack *eval(stack *old_stack)
{
    node *node = peek(old_stack);
    old_stack = pop(old_stack, 1);
    stack *new_stack = empty_stack();
    new_stack = push(new_stack, node);
    print_stack(new_stack, "NEW_STACK");
    new_stack = unwind(new_stack);
    struct node *evaluated_node = peek(new_stack);
    old_stack = push(old_stack, evaluated_node);
    return old_stack;
}

node *eval_node(node *node)
{
    stack *new_stack = empty_stack();
    new_stack = push(new_stack, node);
    print_stack(new_stack, "NEW_STACK");
    new_stack = unwind(new_stack);
    struct node *evaluated_node = peek(new_stack);
    return evaluated_node;
}

/* MAIN */

stack *f_add(stack *stack)
{
    printf("EVALUATING f_add\n");
    print_stack(stack, "F_ADD");

    node *left = eval_node(lookup(stack, 0));
    node *right = eval_node(lookup(stack, 1));

    assert(left->tag == N_INT);
    assert(right->tag == N_INT);
    int sum = left->nint + right->nint;
    node *sum_node = new_node();
    sum_node->tag = N_INT;
    sum_node->nint = sum;

    stack = push(stack, sum_node);
    stack = update(stack, 2);
    stack = pop(stack, 2);
    return stack;
}

stack *f_id(stack *stack)
{
    printf("EVALUATING id\n");
    stack = push_instr(stack, 0);
    stack = update(stack, 1);
    stack = pop(stack, 1);
    return stack;
}
stack *f_twice(stack *stack)
{

    stack = push_instr(stack, 0);
    stack = push_instr(stack, 1);
    stack = push_global(stack, 2, f_add);
    stack = mk_app(stack);
    stack = mk_app(stack);
    stack = update(stack, 1);
    stack = pop(stack, 1);
    return stack;
}

stack *f_fst(stack *stack)
{
    stack = push_instr(stack, 0);
    stack = update(stack, 2);
    stack = pop(stack, 2);
    return stack;
}

stack *f_cons(stack *stack)
{
    stack = pack(stack, 1, 2);
    // stack = push_instr(stack, 0);
    stack = update(stack, 0);
    stack = pop(stack, 0);
    return stack;
}

stack *f_nil(stack *stack)
{
    stack = pack(stack, 0, 0);
    // stack = push_instr(stack, 0);
    stack = update(stack, 0);
    stack = pop(stack, 0);
    return stack;
}

stack *f_five(stack *stack)
{
    printf("EVALUATING five\n");
    stack = push_int(stack, 2);
    stack = push_int(stack, 3);
    stack = push_global(stack, 2, f_add);
    stack = mk_app(stack);
    stack = mk_app(stack);

    print_stack(stack, "BEFORE FIVE UPDATE");
    stack = update(stack, 0);
    print_stack(stack, "AFTER FIVE UPDATE");
    stack = pop(stack, 0);
    return stack;
}

stack *f_addOne(stack *stack)
{
    stack = push_int(stack, 1);
    stack = push_global(stack, 2, f_add);
    stack = mk_app(stack);
    stack = update(stack, 1);
    return pop(stack, 1);
}

stack *f_main(stack *stack)
{
    // id 3
    // stack = push_int(stack, 3);
    // stack = push_global(stack, 1, f_id);
    // stack = mk_app(stack);

    // Nil
    // stack = push_global(stack, 0, f_nil);

    // Cons 42 Nil
    // stack = push_global(stack, 0, f_nil);
    // stack = push_int(stack, 42);
    // stack = push_global(stack, 2, f_cons);
    // stack = mk_app(stack);
    // stack = mk_app(stack);

    // five + five
    // stack = push_global(stack, 0, f_five);
    // // stack = push_global(stack, 0, f_five);
    // stack = push_instr(stack, 0);
    // stack = push_global(stack, 2, f_add);
    // stack = mk_app(stack);
    // stack = mk_app(stack);

    // twice five
    // stack = push_global(stack, 0, f_five);
    // stack = push_global(stack, 1, f_twice);
    // stack = mk_app(stack);

    // twice (id 5)
    // stack = push_int(stack, 5);
    // stack = push_global(stack, 1, f_id);
    // stack = mk_app(stack);
    // stack = push_global(stack, 1, f_twice);
    // stack = mk_app(stack);

    // addOne 4 (does not work)
    // stack = push_int(stack, 4);
    // stack = push_global(stack, 1, f_addOne);
    // stack = mk_app(stack);

    // fst (1 + 2 + 30) 99
    stack = push_int(stack, 99);
    stack = push_int(stack, 30);
    stack = push_int(stack, 2);
    stack = push_int(stack, 1);
    stack = push_global(stack, 2, f_add);
    stack = mk_app(stack);
    stack = mk_app(stack);
    stack = push_global(stack, 2, f_add);
    stack = mk_app(stack);
    stack = mk_app(stack);
    stack = push_global(stack, 2, f_fst);
    stack = mk_app(stack);
    stack = mk_app(stack);

    print_stack(stack, "MAIN BEFORE UPDATING");
    stack = update(stack, 0);
    stack = pop(stack, 0);
    return stack;
}

int main()
{
    stack *stack = empty_stack();
    stack = push_global(stack, 0, f_main);
    print_stack(stack, "INIT");

    // stack = eval(stack);
    stack = unwind(stack);

    print_stack(stack, "FINAL");

    return 0;
}
