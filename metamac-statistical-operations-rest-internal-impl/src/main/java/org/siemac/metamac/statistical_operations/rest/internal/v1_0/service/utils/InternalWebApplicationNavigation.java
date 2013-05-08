package org.siemac.metamac.statistical_operations.rest.internal.v1_0.service.utils;

import java.util.HashMap;
import java.util.Map;

import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.navigation.shared.NameTokens;
import org.siemac.metamac.statistical.operations.navigation.shared.PlaceRequestParams;
import org.springframework.web.util.UriTemplate;

public class InternalWebApplicationNavigation {

    private final String      ANCHOR                 = "#";
    private final String      SEPARATOR              = "/";
    private final String      OPERATION_ID_PARAMETER = "operationIdParam";
    private final String      RESOURCE_ID_PARAMETER  = "resourceIdParam";

    private final UriTemplate familyTemplate;
    private final UriTemplate operationTemplate;
    private final UriTemplate instanceTemplate;

    public InternalWebApplicationNavigation(String webApplicationPath) {
        operationTemplate = new UriTemplate(webApplicationPath + SEPARATOR + ANCHOR + NameTokens.operationListPage + SEPARATOR + NameTokens.operationPage + ";" + PlaceRequestParams.operationParam
                + "=" + "{" + RESOURCE_ID_PARAMETER + "}");
        instanceTemplate = new UriTemplate(webApplicationPath + SEPARATOR + ANCHOR + NameTokens.operationListPage + SEPARATOR + NameTokens.operationPage + ";" + PlaceRequestParams.operationParam
                + "=" + "{" + OPERATION_ID_PARAMETER + "}/" + NameTokens.instancePage + ";" + PlaceRequestParams.instanceParam + "={" + RESOURCE_ID_PARAMETER + "}");
        familyTemplate = new UriTemplate(webApplicationPath + SEPARATOR + ANCHOR + NameTokens.familyListPage + SEPARATOR + NameTokens.familyPage + ";" + PlaceRequestParams.familyParam + "=" + "{"
                + RESOURCE_ID_PARAMETER + "}");
    }

    public String buildOperationUrl(Operation operation) {
        Map<String, String> parameters = new HashMap<String, String>(1);
        parameters.put(RESOURCE_ID_PARAMETER, operation.getCode());
        return operationTemplate.expand(parameters).toString();
    }

    public String buildInstanceUrl(Instance instance) {
        Map<String, String> parameters = new HashMap<String, String>(2);
        parameters.put(OPERATION_ID_PARAMETER, instance.getOperation().getCode());
        parameters.put(RESOURCE_ID_PARAMETER, instance.getCode());
        return instanceTemplate.expand(parameters).toString();
    }

    public String buildFamilyUrl(Family family) {
        Map<String, String> parameters = new HashMap<String, String>(1);
        parameters.put(RESOURCE_ID_PARAMETER, family.getCode());
        return familyTemplate.expand(parameters).toString();
    }

}