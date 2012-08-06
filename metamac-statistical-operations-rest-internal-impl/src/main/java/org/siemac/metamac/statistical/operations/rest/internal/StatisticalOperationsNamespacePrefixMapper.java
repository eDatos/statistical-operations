package org.siemac.metamac.statistical.operations.rest.internal;

import com.sun.xml.bind.marshaller.NamespacePrefixMapper;

public class StatisticalOperationsNamespacePrefixMapper extends NamespacePrefixMapper {

    private static final String STATISTICAL_OPERATIONS_V_1_0_URI    = "http://www.siemac.org/metamac/statistical/operations/rest/internal/v1.0/domain";
    private static final String STATISTICAL_OPERATIONS_V_1_0_PREFIX = "operations";

    private static final String COMMON_V_1_0_URI                    = "http://www.siemac.org/metamac/rest/common/v1.0/domain";
    private static final String COMMON_V_1_0_PREFIX                 = "common";

    @Override
    public String getPreferredPrefix(String namespaceUri, String suggestion, boolean requirePrefix) {
        if (STATISTICAL_OPERATIONS_V_1_0_URI.equals(namespaceUri)) {
            return STATISTICAL_OPERATIONS_V_1_0_PREFIX;
        } else if (COMMON_V_1_0_URI.equals(namespaceUri)) {
            return COMMON_V_1_0_PREFIX;
        }
        return suggestion;
    }

    @Override
    public String[] getPreDeclaredNamespaceUris() {
        return new String[]{STATISTICAL_OPERATIONS_V_1_0_URI, COMMON_V_1_0_URI};
    }

}