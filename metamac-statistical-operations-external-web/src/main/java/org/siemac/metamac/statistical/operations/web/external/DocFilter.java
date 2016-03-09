package org.siemac.metamac.statistical.operations.web.external;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;

public class DocFilter implements Filter {

    private static Pattern swaggerVersionPattern = Pattern.compile(".*(v\\d+\\.\\d+).swagger.jsp");

    private String[]       supportedVersions     = new String[]{"/v1.0/"};

    @Override
    public void init(FilterConfig config) throws ServletException {
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws java.io.IOException, ServletException {
        String path = ((HttpServletRequest) request).getRequestURI();
        if (StringUtils.endsWithAny(path, supportedVersions)) {
            request.getRequestDispatcher("/docs/api/index.jsp").forward(request, response);
        } else if (path.endsWith("swagger.jsp")) {
            Matcher matcher = swaggerVersionPattern.matcher(path);
            if (matcher.matches() && matcher.groupCount() >= 0) {
                request.getRequestDispatcher("/docs/api/" + matcher.group(1) + "/swagger.jsp").forward(request, response);
            }
        } else {
            chain.doFilter(request, response);
        }
    }

    @Override
    public void destroy() {
    }
}
