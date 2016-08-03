package org.siemac.metamac.statistical.operations.web.external.urlrewrite;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.tuckey.web.filters.urlrewrite.extend.RewriteMatch;
import org.tuckey.web.filters.urlrewrite.extend.RewriteRule;

public class StatisticalOperationsRewriteRule extends RewriteRule {

    public RewriteMatch matches(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) {
        return new StatisticalOperationsRewriteMatch();
    }
}
