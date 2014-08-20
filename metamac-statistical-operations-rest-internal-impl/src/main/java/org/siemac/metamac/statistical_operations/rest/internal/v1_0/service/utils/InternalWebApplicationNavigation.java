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

    private final String ANCHOR = "#";
    private final String SEPARATOR = "/";
    private final String OPERATION_ID_PARAMETER = "operationIdParam";
    private final String RESOURCE_ID_PARAMETER = "resourceIdParam";

    private final UriTemplate familyTemplate;
    private final UriTemplate operationTemplate;
    private final UriTemplate instanceTemplate;

    public InternalWebApplicationNavigation(String webApplicationPath) {
        this.operationTemplate = new UriTemplate(webApplicationPath + this.SEPARATOR + this.ANCHOR + NameTokens.operationListPage + this.SEPARATOR + NameTokens.operationPage + ";"
                + PlaceRequestParams.operationParam + "=" + "{" + this.RESOURCE_ID_PARAMETER + "}");
        this.instanceTemplate = new UriTemplate(webApplicationPath + this.SEPARATOR + this.ANCHOR + NameTokens.operationListPage + this.SEPARATOR + NameTokens.operationPage + ";"
                + PlaceRequestParams.operationParam + "=" + "{" + this.OPERATION_ID_PARAMETER + "}/" + NameTokens.instancePage + ";" + PlaceRequestParams.instanceParam + "={"
                + this.RESOURCE_ID_PARAMETER + "}");
        this.familyTemplate = new UriTemplate(webApplicationPath + this.SEPARATOR + this.ANCHOR + NameTokens.familyListPage + this.SEPARATOR + NameTokens.familyPage + ";"
                + PlaceRequestParams.familyParam + "=" + "{" + this.RESOURCE_ID_PARAMETER + "}");
    }

    public String buildOperationUrl(String operationCode) {
        Map<String, String> parameters = new HashMap<String, String>(1);
        parameters.put(this.RESOURCE_ID_PARAMETER, operationCode);
        return this.operationTemplate.expand(parameters).toString();
    }

    public String buildOperationUrl(Operation operation) {
        return this.buildOperationUrl(operation.getCode());
    }

    public String buildInstanceUrl(Instance instance) {
        Map<String, String> parameters = new HashMap<String, String>(2);
        parameters.put(this.OPERATION_ID_PARAMETER, instance.getOperation().getCode());
        parameters.put(this.RESOURCE_ID_PARAMETER, instance.getCode());
        return this.instanceTemplate.expand(parameters).toString();
    }

    public String buildFamilyUrl(Family family) {
        Map<String, String> parameters = new HashMap<String, String>(1);
        parameters.put(this.RESOURCE_ID_PARAMETER, family.getCode());
        return this.familyTemplate.expand(parameters).toString();
    }

}