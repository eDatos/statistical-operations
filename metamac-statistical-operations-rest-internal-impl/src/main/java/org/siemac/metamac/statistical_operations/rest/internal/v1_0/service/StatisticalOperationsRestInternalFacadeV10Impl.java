package org.siemac.metamac.statistical_operations.rest.internal.v1_0.service;

import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response.Status;

import org.apache.cxf.jaxrs.ext.MessageContext;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.fornax.cartridges.sculptor.framework.domain.PagingParameter;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.aop.LoggingInterceptor;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.rest.exception.RestCommonServiceExceptionType;
import org.siemac.metamac.rest.exception.RestException;
import org.siemac.metamac.rest.exception.utils.RestExceptionUtils;
import org.siemac.metamac.rest.search.criteria.SculptorCriteria;
import org.siemac.metamac.rest.utils.RestUtils;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.FamilyProperties;
import org.siemac.metamac.statistical.operations.core.domain.InstanceProperties;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsListsService;
import org.siemac.metamac.statistical_operations.rest.internal.exception.RestServiceExceptionType;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.CollMethods;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Costs;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Families;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.InstanceTypes;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Instances;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.OfficialityTypes;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Operations;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.SurveySources;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.SurveyTypes;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.mapper.Do2RestInternalMapperV10;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.mapper.RestCriteria2SculptorCriteriaMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service("statisticalOperationsRestInternalFacadeV10")
public class StatisticalOperationsRestInternalFacadeV10Impl implements StatisticalOperationsRestInternalFacadeV10 {

    @Autowired
    private StatisticalOperationsBaseService    statisticalOperationsBaseService;

    @Autowired
    private StatisticalOperationsListsService   statisticalOperationsListsService;

    @Autowired
    private Do2RestInternalMapperV10            do2RestInternalMapper;

    @Autowired
    private RestCriteria2SculptorCriteriaMapper restCriteria2SculptorCriteriaMapper;

    // @Context // must inject with setter, because with @Context is not injected in web server
    private MessageContext                      context;

    private ServiceContext                      serviceContextRestInternal = new ServiceContext("restInternal", "restInternal", "restInternal");
    private Logger                              logger                     = LoggerFactory.getLogger(LoggingInterceptor.class);

    @Context
    public void setMessageContext(MessageContext context) {
        this.context = context;
    }

    @Override
    public Operation retrieveOperationById(String id) {
        try {
            // Retrieve
            org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = retrieveOperationEntityPublishedInternalOrExternally(id);

            // Transform
            Operation operation = do2RestInternalMapper.toOperation(operationEntity, getApiUrl());
            return operation;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Operations findOperations(String query, String orderBy, String limit, String offset) {
        try {
            // Retrieve operations by criteria
            SculptorCriteria sculptorCriteria = restCriteria2SculptorCriteriaMapper.getOperationCriteriaMapper().restCriteriaToSculptorCriteria(query, orderBy, limit, offset);
            // Find only published
            ConditionalCriteria conditionalCriteriaProcStatus = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Operation.class)
                    .withProperty(OperationProperties.procStatus()).in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).buildSingle();

            List<ConditionalCriteria> conditionalCriteria = new ArrayList<ConditionalCriteria>();
            conditionalCriteria.add(conditionalCriteriaProcStatus);
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> operationsEntitiesResult = statisticalOperationsBaseService.findOperationByCondition(
                    serviceContextRestInternal, conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            Operations operations = do2RestInternalMapper.toOperations(operationsEntitiesResult, query, orderBy, sculptorCriteria.getLimit(), getApiUrl());
            return operations;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Instances findInstances(String operationId, String query, String orderBy, String limit, String offset) {
        try {
            // Retrieve operation to check exists and it is published
            org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = retrieveOperationEntityPublishedInternalOrExternally(operationId);

            // Retrieve instances by criteria
            SculptorCriteria sculptorCriteria = restCriteria2SculptorCriteriaMapper.getInstanceCriteriaMapper().restCriteriaToSculptorCriteria(query, orderBy, limit, offset);
            // Find for operation and only published
            List<ConditionalCriteria> conditionalCriteriaOperationAndProcStatus = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Instance.class)
                    .withProperty(InstanceProperties.operation().code()).eq(operationId).withProperty(InstanceProperties.procStatus())
                    .in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).build();

            List<ConditionalCriteria> conditionalCriteria = new ArrayList<ConditionalCriteria>();
            conditionalCriteria.addAll(conditionalCriteriaOperationAndProcStatus);
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Instance> instancesEntitiesResult = statisticalOperationsBaseService.findInstanceByCondition(serviceContextRestInternal,
                    conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            Instances instances = do2RestInternalMapper.toInstances(operationEntity, instancesEntitiesResult, query, orderBy, sculptorCriteria.getLimit(), getApiUrl());
            return instances;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Families retrieveFamiliesByOperation(String id) {
        try {
            // Retrieve operation to check exists and it is published
            retrieveOperationEntityPublishedInternalOrExternally(id);

            // Retrieve families by criteria
            List<ConditionalCriteria> conditionalCriteria = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Family.class)
                    .withProperty(FamilyProperties.operations().code()).eq(id).withProperty(FamilyProperties.procStatus()).in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY)
                    .distinctRoot().build();
            PagingParameter pagingParameter = PagingParameter.noLimits();
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> familiesEntitiesResult = statisticalOperationsBaseService.findFamilyByCondition(serviceContextRestInternal,
                    conditionalCriteria, pagingParameter);

            // Transform
            Families families = do2RestInternalMapper.toFamiliesByOperation(familiesEntitiesResult.getValues(), getApiUrl());
            return families;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Family retrieveFamilyById(String id) {
        try {
            // Retrieve
            org.siemac.metamac.statistical.operations.core.domain.Family familyEntity = retrieveFamilyEntityPublishedInternalOrExternally(id);

            // Transform
            Family family = do2RestInternalMapper.toFamily(familyEntity, getApiUrl());
            return family;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Families findFamilies(String query, String orderBy, String limit, String offset) {
        try {
            // Retrieve families by criteria
            SculptorCriteria sculptorCriteria = restCriteria2SculptorCriteriaMapper.getFamilyCriteriaMapper().restCriteriaToSculptorCriteria(query, orderBy, limit, offset);
            // Find only published
            ConditionalCriteria conditionalCriteriaProcStatus = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Family.class)
                    .withProperty(FamilyProperties.procStatus()).in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).buildSingle();

            List<ConditionalCriteria> conditionalCriteria = new ArrayList<ConditionalCriteria>();
            conditionalCriteria.add(conditionalCriteriaProcStatus);
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> familiesEntitiesResult = statisticalOperationsBaseService.findFamilyByCondition(serviceContextRestInternal,
                    conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            Families families = do2RestInternalMapper.toFamilies(familiesEntitiesResult, query, orderBy, sculptorCriteria.getLimit(), getApiUrl());
            return families;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Operations findOperationsByFamily(String id, String query, String orderBy, String limit, String offset) {
        try {
            // Validate family exists and it is published
            org.siemac.metamac.statistical.operations.core.domain.Family family = retrieveFamilyEntityPublishedInternalOrExternally(id);

            // Retrieve operations by criteria
            SculptorCriteria sculptorCriteria = restCriteria2SculptorCriteriaMapper.getOperationCriteriaMapper().restCriteriaToSculptorCriteria(query, orderBy, limit, offset);
            // Find only this family and published
            List<ConditionalCriteria> conditionalCriteria = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Operation.class)
                    .withProperty(OperationProperties.families().code()).eq(id).withProperty(OperationProperties.procStatus()).in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY)
                    .build();
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> operationsEntitiesResult = statisticalOperationsBaseService.findOperationByCondition(
                    serviceContextRestInternal, conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            Operations operations = do2RestInternalMapper.toOperationsByFamily(family, operationsEntitiesResult, query, orderBy, sculptorCriteria.getLimit(), getApiUrl());
            return operations;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Instance retrieveInstanceById(String operationId, String id) {
        try {
            org.siemac.metamac.statistical.operations.core.domain.Instance instanceEntity = statisticalOperationsBaseService.findInstanceByCode(serviceContextRestInternal, id);
            if (instanceEntity == null || (!ProcStatusEnum.PUBLISH_EXTERNALLY.equals(instanceEntity.getProcStatus()) && !ProcStatusEnum.PUBLISH_INTERNALLY.equals(instanceEntity.getProcStatus()))) {
                // Instance not found
                org.siemac.metamac.rest.common.v1_0.domain.Exception exception = RestExceptionUtils.getException(RestServiceExceptionType.INSTANCE_NOT_FOUND, id);
                throw new RestException(exception, Status.NOT_FOUND);
            }

            // Transform and return
            Instance instance = do2RestInternalMapper.toInstance(instanceEntity, getApiUrl());
            return instance;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public SurveyTypes retrieveSurveyTypes() {
        try {
            // Retrieve all
            List<SurveyType> entitiesResult = statisticalOperationsListsService.findAllSurveyTypes(serviceContextRestInternal);

            // Transform
            SurveyTypes surveyTypes = do2RestInternalMapper.toSurveyTypes(entitiesResult);
            return surveyTypes;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public OfficialityTypes retrieveOfficialityTypes() {
        try {
            // Retrieve all
            List<OfficialityType> entitiesResult = statisticalOperationsListsService.findAllOfficialityTypes(serviceContextRestInternal);

            // Transform
            OfficialityTypes officialityTypes = do2RestInternalMapper.toOfficialityTypes(entitiesResult);
            return officialityTypes;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public InstanceTypes retrieveInstanceTypes() {
        try {
            // Retrieve all
            List<InstanceType> entitiesResult = statisticalOperationsListsService.findAllInstanceTypes(serviceContextRestInternal);

            // Transform
            InstanceTypes instanceTypes = do2RestInternalMapper.toInstanceTypes(entitiesResult);
            return instanceTypes;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public SurveySources retrieveSurveySources() {
        try {
            // Retrieve all
            List<SurveySource> entitiesResult = statisticalOperationsListsService.findAllSurveySources(serviceContextRestInternal);

            // Transform
            SurveySources surveySources = do2RestInternalMapper.toSurveySources(entitiesResult);
            return surveySources;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public CollMethods retrieveCollMethods() {
        try {
            // Retrieve all
            List<CollMethod> entitiesResult = statisticalOperationsListsService.findAllCollMethods(serviceContextRestInternal);

            // Transform
            CollMethods collMethods = do2RestInternalMapper.toCollMethods(entitiesResult);
            return collMethods;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Costs retrieveCosts() {
        try {
            // Retrieve all
            List<Cost> entitiesResult = statisticalOperationsListsService.findAllCosts(serviceContextRestInternal);

            // Transform
            Costs costs = do2RestInternalMapper.toCosts(entitiesResult);
            return costs;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    /**
     * Retrieve operation by code (id in Api) and check it is published externally or internally
     */
    private org.siemac.metamac.statistical.operations.core.domain.Operation retrieveOperationEntityPublishedInternalOrExternally(String id) throws MetamacException {
        org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = statisticalOperationsBaseService.findOperationByCode(serviceContextRestInternal, id);
        if (operationEntity == null || (!ProcStatusEnum.PUBLISH_EXTERNALLY.equals(operationEntity.getProcStatus()) && !ProcStatusEnum.PUBLISH_INTERNALLY.equals(operationEntity.getProcStatus()))) {
            // Operation not found
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = RestExceptionUtils.getException(RestServiceExceptionType.OPERATION_NOT_FOUND, id);
            throw new RestException(exception, Status.NOT_FOUND);

        }
        return operationEntity;
    }

    /**
     * Retrieve family by code (id in Api) and check it is published externally or internally
     */
    private org.siemac.metamac.statistical.operations.core.domain.Family retrieveFamilyEntityPublishedInternalOrExternally(String id) throws MetamacException {
        org.siemac.metamac.statistical.operations.core.domain.Family familyEntity = statisticalOperationsBaseService.findFamilyByCode(serviceContextRestInternal, id);

        if (familyEntity == null || (!ProcStatusEnum.PUBLISH_EXTERNALLY.equals(familyEntity.getProcStatus()) && !ProcStatusEnum.PUBLISH_INTERNALLY.equals(familyEntity.getProcStatus()))) {
            // Family not found or not published
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = RestExceptionUtils.getException(RestServiceExceptionType.FAMILY_NOT_FOUND, id);
            throw new RestException(exception, Status.NOT_FOUND);

        }
        return familyEntity;
    }

    /**
     * Get Base API url
     */
    private String getApiUrl() {
        return RestUtils.getApiUrl(context);
    }

    /**
     * Throws response error, logging exception
     */
    private RestException manageException(Exception e) {
        logger.error("Error", e);
        if (e instanceof RestException) {
            return (RestException) e;
        } else {
            // do not show information details about exception to user
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = RestExceptionUtils.getException(RestCommonServiceExceptionType.UNKNOWN);
            return new RestException(exception, Status.INTERNAL_SERVER_ERROR);
        }
    }
}