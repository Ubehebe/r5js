import {Value} from "../value";

/**
 * Interface abstracted from {@link r5js.Environment}.
 *
 * An Environment stores three common kinds of objects:
 * - Datums (most Scheme values: numbers, identifiers, etc.)
 * - SchemeProcedures (native Scheme procedures)
 * - JavaScript functions ('primitive' Scheme procedures)
 *
 * There is a fourth kind of object, a Continuation, which can get stored
 * when calling "magical" procedures like call/cc, where the current
 * continuation is bound to a formal parameter.
 *
 * {@link IEnvironment#get} will only ever return Datums and Continuations;
 * it will wrap SchemeProcedures and JavaScript functions in Datum
 * wrappers before returning, to allow for things like
 *
 * (cons + (lambda () "hi!"))
 *
 * A drawback is that comparisons on stuff retrieved from an Environment
 * may need to be unwrapped:
 *
 * let x = env.get('foo');
 * let y = env.get('foo');
 * x == y // false if foo is a SchemeProcedure or JavaScript function!
 *
 * If you know your key should retrieve a SchemeProcedure or JavaScript
 * function, you can use {@link r5js.IEnvironment.getProcedure} to avoid the
 * wrapping and unwrapping.
 *
 */
export interface Environment {
    addBinding(name: string, val: Value);

    /**
     * Used exclusively during desugaring of lambda expressions.
     *
     * Lambda expressions have much in common with procedure definitions,
     * even though they don't introduce a new binding (in a programmer-visible
     * way, at least). For example:
     *
     * (define (foo x) (define (bar y) (+ x y)) bar)
     *
     * (define (foo x) (lambda (y) (+ x y)))
     *
     * With either definition of foo, we must have
     *
     * ((foo 10) 11) => 22
     *
     * With internal definitions, this is easy. The grammar of Scheme says that
     * all internal definitions must precede all expressions in a procedure body,
     * so the {@link UserDefinedProcedure} constructor can intercept all the
     * definitions and deal with them appropriately.
     *
     * Lambda expressions, however, can appear anywhere in a procedure's body,
     * so we deal with them in a generic way here. Using the second definition of
     * foo above as an example, here's what happens:
     *
     * - During parsing of foo, we create a new {@link IEnvironment}
     *   for the procedure (say, fooEnv), and note all foo's lambdas in fooEnv,
     *   using {@link IEnvironment#addClosure}.
     * - Later, when we want to evaluate (foo 10), we create a new
     *   {@link IEnvironment} hanging off fooEnv (say, tmp-fooEnv).
     *   (We have to do this to support multiple active calls to the same
     *   procedure.) We copy all of fooEnv's closures into tmp-fooEnv as actual
     *   bound {@link UserDefinedProcedure}s, using
     *   {@link r5js.Environment.addClosuresFrom}.
     *   We also bind the arguments (in this case x = 10) in tmp-fooEnv,
     *   then advance to foo's body.
     *
     * In this way, when we get to the body of the lambda expression, both x and y
     * are already in scope. The key point is that the environment
     * of (lambda (y) (+ x y)) points back to the environment representing the
     * _execution_ of foo (tmp-fooEnv), not the Environment representing foo itself
     * (fooEnv).
     *
     * TODO bl: consider renaming to addSchemeProcedure?
     */
    addClosure(name: string, proc: any /* TODO UserDefinedProcedure */);

    /** @returns This environment, for chaining. */
    addClosuresFrom(other: Environment): this;

    /**
     * @param otherEnv Environment whose closures this environment should use.
     * TODO bl: this method is only used once. Can I eliminate it?
     */
    setClosuresFrom(otherEnv: Environment);

    get(name: string): Value|null;

    getProcedure(name: string): Value|null;

    /**
     * @returns true iff the environment, or any of its enclosing
     * environments, has a binding for the name.
     */
    hasBindingRecursive(name: string): boolean;

    /**
     * R5RS 5.2.1: "At the top level of a program, a definition
     *
     * (define <variable> <expression>)
     *
     * has essentially the same effect as the assignment expression
     *
     * (set! <variable> <expression>)
     *
     * if <variable> is bound. If <variable> is not bound, however, then
     * the definition will bind <variable> to a new location before performing
     * the assignment, whereas it would be an error to perform a set! on
     * an unbound variable."
     *
     * We use the isTopLevel parameter to perform the override mentioned.
     */
    mutate(name: string, newVal: Value, isTopLevel: boolean);

    /**
     * Just for environments defined in the standard; users shouldn't be able to
     * add to them.
     */
    seal();

    child(): Environment;

    /** @returns this object, for chaining. */
    allowRedefs(): this;
}