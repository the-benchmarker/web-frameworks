package org.apache.struts.benchmarker.example;

import com.opensymphony.xwork2.ActionInvocation;
import com.opensymphony.xwork2.Result;

import javax.servlet.http.HttpServletResponse;
import java.io.StringWriter;

/**
 * Remove once Struts 2.6 will be available
 */
public interface PlainResult extends Result {

    @Override
    default void execute(ActionInvocation invocation) throws Exception {
        StringWriter body = new StringWriter();
        write(body);

        HttpServletResponse response = invocation.getInvocationContext().getServletResponse();

        response.getWriter().write(body.toString());
        response.flushBuffer();
    }

    void write(StringWriter body);

}
